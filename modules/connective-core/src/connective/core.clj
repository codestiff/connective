(ns connective.core
  (:require
   [taoensso.timbre :as log]
   [connective.adapter :as adapter]
   [connective.schema :as schema]
   [connective.entity :as entity]))

(defmacro defn-of-adaptor
  [sym]
  `(defn ~sym
     [~'adapter ~'context ~'arg]
     (~(symbol "adapter" (str sym)) ~'adapter ~'context ~'arg)))

(defmacro defn-of-adaptor-2
  [sym log-fn]
  `(defn ~sym
     [~'adapter ~'context ~'arg]
     (let [r# (~(symbol "adapter" (str sym)) ~'adapter ~'context ~'arg)]
       (~log-fn (str '~sym) ~'arg r#)
       r#)))

(defn- log-read
  [op args result]
  (let [p (select-keys args [::entity/kind ::entity/id])]
    (log/debug :op op :ident p)))

(defn- log-delete
  [op args result]
  (let [p (select-keys result [::entity/kind ::entity/id])]
    (log/debug :op op :ident p)))

(defn- log-write
  [op args result]
  (let [p (select-keys result [::entity/ident])]
    (log/debug :op op :entity p)))

(defn-of-adaptor related-query)
(defn-of-adaptor reference-query)
(defn-of-adaptor execute-query)
(defn-of-adaptor init-entity)
(defn-of-adaptor-2 write-entity log-write)
(defn-of-adaptor-2 read-entity log-read)
(defn-of-adaptor-2 delete-entity log-delete)
(defn-of-adaptor reference-value)
(defn validator
  [a]
  (adapter/validator a))

(def attributes entity/attributes-of-entity)
(def context entity/context-of-entity)
(def ident entity/ident-of-entity)
(def relationships entity/relationships-of-entity)

(def compile-schema schema/compile)

(defmacro def-collection-fn
  [sym-plural sym-singular]
  `(defn ~sym-plural
     [~'adapter ~'context ~'coll]
     (for [~'item ~'coll]
      (~sym-singular ~'adapter ~'context ~'item))))

(def-collection-fn init-entities init-entity)
(def-collection-fn write-entities write-entity)
(def-collection-fn read-entities read-entity)
(def-collection-fn delete-entities delete-entity)

(defn update-rels
  [entity
   upfn]
  (let [new-rels (upfn (entity/relationships-of-entity entity))]
    (entity/assoc-relationships entity new-rels)))

(defn update-attrs
  [entity
   upfn]
  (let [new-attrs (upfn (entity/attributes-of-entity entity))]
    (entity/assoc-attributes entity new-attrs)))

(defn- assoc-parent-relationship
  [entity
   context
   {::keys [parent rel-opts]}]
  (let [relation-key (entity/relationship-key-of-reference-relation-of-entity
                      context
                      entity
                      rel-opts)]
    (assoc-in entity [::relationships relation-key] parent)))

(defn walk-rel-tree
  [a
   {::entity/keys [entity
                   parent
                   parent-relation]
    ::keys [node-fn
            filter-fn]
    :or {filter-fn (fn [_ _ {::entity/keys [related-entity]}]
                     (nil? related-entity))}
    :as context}]
  (let [relationships (entity/relationship-groups-of-entity context entity)
        entity (reduce
                (fn
                  [e*
                   {::entity/keys [relationship-key
                                   related-entity]
                    :as rel-ctx}]
                  (if (filter-fn a context rel-ctx)
                    e*
                    (let [context (->
                                   context
                                   (assoc ::entity/entity related-entity)
                                   (update ::entity/rel-path (fnil conj []) relationship-key)
                                   (dissoc
                                    ::entity/parent
                                    ::entity/parent-relation))
                          related-entity (walk-rel-tree a context)]
                      ;; unsure, but maybe set the corresponding
                      ;; many relationship in the related entity to
                      ;; ::entity/parent instead of having it not
                      ;; exist, in order to tell if it should be
                      ;; validated or not
                      (assoc-in e* [::entity/relationships relationship-key] related-entity))))
                entity
                (get relationships ::entity/reference))

        [rel-type {::entity/keys [ref-attribute] :as rel-opts}] parent-relation

        entity (case rel-type
                 (::entity/reference,nil) (node-fn
                                           a
                                           context
                                           entity)
                 ::entity/many (let [parent-rel-key (entity/relationship-key-of-reference-relation-of-entity
                                                     context
                                                     entity
                                                     rel-opts)
                                     _ (assert (some? parent-rel-key))
                                     entity (assoc-in entity [::entity/relationships parent-rel-key] parent)
                                     entity (node-fn a context entity)]
                                 (assert (some? ref-attribute))
                                 (->
                                  entity
                                  (assoc-in [::entity/relationships parent-rel-key] ::entity/parent)
                                  (assoc-in [::entity/attributes ref-attribute] ::entity/parent))))]
    (reduce
     (fn
       [e*
        {::entity/keys [relationship-key
                        relation
                        related-entity]
         :as rel-ctx}]
       (if (filter-fn a context rel-ctx)
         e*
         (let [context (->
                        context
                        (update ::entity/rel-path (fnil conj []) relationship-key)
                        (assoc
                         ::entity/parent entity
                         ::entity/parent-relation relation))
               related-entities (doall
                                 (for [e related-entity]
                                   (walk-rel-tree
                                    a
                                    (assoc context ::entity/entity e))))]
           (assoc-in e* [::entity/relationships relationship-key] related-entities))))
     entity
     (get relationships ::entity/many))))

(defn- init-rels*
  [a
   context
   entity]
  (walk-rel-tree
   a
   (assoc
    context
    ::node-fn init-entity
    ::entity/entity entity)))

(defn init-rels
  [a
   context
   entity]
  (init-rels* a context entity))

(defn- write-rels*
  [a
   context
   entity]
  (walk-rel-tree
   a
   (assoc
    context
    ::node-fn write-entity
    ::entity/entity entity)))

(defn write-rels
  [a
   context
   entity]
  (write-rels* a context entity))

(defn pull-rel-tree
  [a
   {::entity/keys [entity
                   parent
                   parent-relation]
    ::keys [query-rels]
    :as context}]
  (let [relationships (entity/relationship-groups-of-entity context entity)
        query-rels (::entity/relationships query-rels)
        content (assoc context ::entity/parent entity)
        entity (reduce
                (fn
                  [e*
                   {::entity/keys [relationship-key
                                   relation]}]
                  (if (contains? query-rels relationship-key)
                    (let [[_ {::entity/keys [ref-attribute]
                              :as rel-opts}] relation
                          _ (assert (some? ref-attribute))
                          ref-value (get-in e* [::entity/attributes ref-attribute])
                          related-entity (->>
                                          (assoc rel-opts ::entity/ref-value ref-value)
                                          (reference-query a context)
                                          (execute-query a context))
                          context (->
                                   context
                                   (assoc
                                    ::query-rels (get query-rels relationship-key)
                                    ::entity/parent-relation relation
                                    ::entity/entity related-entity))
                          related-entity (pull-rel-tree a context)]
                      (assoc-in e* [::entity/relationships relationship-key] related-entity))

                    e*))
                entity
                (get relationships ::entity/reference))
        ref-value (reference-value a context entity)]
    (reduce
     (fn
       [e*
        {::entity/keys [relationship-key
                        relation
                        related-entity]
         :as ctx}]
       (if (contains? query-rels relationship-key)
         (let [[_ {::entity/keys [ref-attribute]
                   :as rel-opts}] relation
               _ (assert (some? ref-attribute))
               related-entities (->>
                                 (assoc rel-opts ::entity/ref-value ref-value)
                                 (related-query a context)
                                 (execute-query a context))
               context (->
                        context
                        (assoc
                         ::query-rels (get query-rels relationship-key)
                         ::entity/parent-relation relation))
               related-entities (doall
                                 (for [e related-entities]
                                   (let [rel-key (entity/relationship-key-of-reference-relation-of-entity
                                                         context
                                                         entity
                                                         rel-opts)
                                         e (pull-rel-tree
                                            a
                                            (assoc context ::entity/entity e))]
                                     (->
                                      e
                                      (assoc-in [::entity/attributes ref-attribute] ::entity/parent)
                                      (assoc-in [::entity/relationships rel-key] ::entity/parent)))))]
           (assoc-in e* [::entity/relationships relationship-key] related-entities))
         e*))
     entity
     (get relationships ::entity/many))))

(defn- read-rels*
  [a
   context
   ident]
  (pull-rel-tree
   a
   (assoc
    context
    ::entity/entity (read-entity a context ident))))

(defn read-rels
  [a
   context
   {::entity/keys [relationships]
    :as ident}]
  (read-rels*
   a
   (assoc
    context
    ::query-rels {::entity/relationships relationships})
   (dissoc ident ::entity/relationships)))

(defn- delete-rels*
  [a
   context
   {::keys [delete-all]
    :as entity}]
  (walk-rel-tree
   a
   (assoc
    context
    ::node-fn (fn
                [a ctx entity]
                (delete-entity a ctx (entity/ident-of-entity entity))
                entity)
    ::filter-fn (fn [_ {::entity/keys [rel-path]
                        ::keys [delete-all]}
                     {::entity/keys [related-entity
                                     relationship-key]}]
                  (let [q-path (mapcat
                                vector
                                rel-path
                                (repeat (count rel-path) ::entity/relationships))
                        delete-rels (get-in delete-all q-path)]
                    (or
                     (nil? related-entity)
                     (not (contains? delete-rels relationship-key)))))
    ::delete-all delete-all
    ::entity/entity (dissoc entity ::delete-all))))

(defn delete-rels
  [a
   context
   ident]
  (delete-rels* a context ident))

(comment
  (macroexpand-1 '(defn-of-adaptor-2 related-query log-op))
  (macroexpand-1 '(def-collection-fn write-entities write-entity))
  )
