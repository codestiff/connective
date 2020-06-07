(ns connective.core
  (:require
   [connective.adapter :as adapter]
   [connective.schema :as schema]
   [connective.entity :as entity]))

(defmacro defn-of-adaptor
  [sym]
  `(defn ~sym
     [~'adapter ~'context ~'arg]
     (~(symbol "adapter" (str sym)) ~'adapter ~'context ~'arg)))

(defn-of-adaptor related-query)
(defn-of-adaptor reference-query)
(defn-of-adaptor execute-query)
(defn-of-adaptor init-entity)
#_(defn init-entity
  [a c p]
  (let [se (entity/simple-entity p)]
    (prn se)
    (adapter/init-entity a c p)))
(defn-of-adaptor write-entity)
(defn-of-adaptor read-entity)
(defn-of-adaptor delete-entity)
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
    ::keys [node-fn]
    :as context}]
  (let [relationships (entity/relationship-groups-of-entity context entity)
        entity (reduce
                (fn
                  [e*
                   {::entity/keys [relationship-key
                                  related-entity]}]
                  (let [context (->
                                 context
                                 (assoc ::entity/entity related-entity)
                                 (dissoc
                                  ::entity/parent
                                  ::entity/parent-relation))
                        related-entity (walk-rel-tree a context)]
                    ;; unsure, but maybe set the corresponding
                    ;; many relationship in the related entity to
                    ;; ::entity/parent instead of having it not
                    ;; exist, in order to tell if it should be
                    ;; validated or not
                    (assoc-in e* [::entity/relationships relationship-key] related-entity)))
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
         :as ctx}]
       (let [context (assoc
                      context
                      ::entity/parent entity
                      ::entity/parent-relation relation)
             related-entities (doall
                               (for [e related-entity]
                                 (walk-rel-tree
                                  a
                                  (assoc context ::entity/entity e))))]
         (assoc-in e* [::entity/relationships relationship-key] related-entities)))
     entity
     (get relationships ::entity/many))))

(defn- init-rels*
  ([a
    context
    entity]
   (walk-rel-tree
    a
    (assoc
     context
     ::node-fn init-entity
     ::entity/entity entity))))

(defn init-rels
  ([a context entity]
   (init-rels a context entity nil))
  ([a
    {::entity/keys [parent]
     :as context}
    entity
    params]
   (init-rels* a (assoc context ::params params) entity)))

(defn- write-rels*
  ([a
    context
    entity]
   (walk-rel-tree
    a
    (assoc
     context
     ::node-fn write-entity
     ::entity/entity entity))))

(defn write-rels
  ([a context entity]
   (write-rels a context entity nil))
  ([a
    {::entity/keys [parent]
     :as context}
    entity
    params]
   (write-rels* a (assoc context ::params params) entity)))

(comment
  (macroexpand-1 '(defn-of-adaptor related-query))
  (macroexpand-1 '(def-collection-fn write-entities write-entity))
  )
