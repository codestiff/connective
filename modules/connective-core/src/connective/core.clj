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

(defn- init-rel
  [a
   {::entity/keys [relation
                   relationship-key]
    ::keys [recur-fn]
    :as context}
   entity
   params]
  (assert
   (and
    (some? relation)
    (some? relationship-key)
    (some? recur-fn)))
  (let [[rel-type {::entity/keys [kind ref-attribute]}] relation]
    (if (or (nil? params)
            (contains? params relationship-key))
      (let [params* (get params relationship-key)]
        (case rel-type
          ::entity/many (doall
                         (for [e entity]
                           (recur-fn
                            a
                            (assoc
                             context
                             ::entity/ref-attribute ref-attribute)
                            e
                            params*)))
          ::entity/reference (recur-fn
                              a
                              (dissoc context ::entity/parent ::entity/ref-attribute)
                              entity
                              params*)))
      entity)))

(defn init-rels
  ([a context entity]
   (init-rels a context entity nil))
  ([a
    {::entity/keys [parent]
     :as context}
    entity
    params]
   (let [;;entity (assoc-parent-reference entity 
         entity-with-rels (update-rels
                           entity
                           (fn
                             [rels]
                             (reduce
                              (fn
                                [rels* [k related-entity]]
                                (let [kind (entity/kind-of-entity entity)
                                      relation (entity/relation-of-entity context entity k)
                                      related-params (get params kind)
                                      context (assoc
                                               context
                                               ::entity/parent entity
                                               ::entity/relation relation
                                               ::entity/relationship-key k
                                               ::recur-fn init-rels)
                                      ;; note, related-entity might be multiple
                                      related-entity (init-rel a context related-entity related-params)]
                                  (assoc rels* k related-entity)))
                              {}
                              rels)))]
     (init-entity
      a
      context
      entity-with-rels))))

(comment
  (macroexpand-1 '(defn-of-adaptor related-query))
  (macroexpand-1 '(def-collection-fn write-entities write-entity))
  )
