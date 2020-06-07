(ns connective.firestore.entity.query
  (:require
   [connective.entity :as entity]
   [connective.firestore.query :as query]
   [firestore-clj.core :as f]))

(defmulti compile-clause
  (fn [_ [op & xs]] op))

(defmethod compile-clause :default
  [context params]
  (query/compile-clause context params))

(defmethod compile-clause :rel=
  [a
   {:keys [schema]
    ::entity/keys [kind]
    :as context}
   [_ rel-key rel-entity]]
  (let [entity-schema nil #_(utils/schema-of-kind schema kind)
        [rel-type relation-params] nil #_(entity/relation-of-schema
                                    entity-schema
                                    rel-key)
        _ (assert (= rel-type ::entity/reference))
        ref-attribute (::entity/ref-attribute relation-params)
        ref-kind (::entity/kind relation-params)
        related-kind nil #_(entity/entity-kind rel-entity)
        _ (assert (= related-kind ref-kind))
        ref-value nil #_(entity/doc-ref context rel-entity)]
    (query/compile-clause
     context
     [:= ref-attribute ref-value])))

(defn- compile-clauses
  [compile-context where]
  (fn
    [a runtime-context]
    (let [context (merge compile-context runtime-context)]
      (reduce
       compile-clause
       context
       where))))

(defn compile
  [{::entity/keys [kind]
    ::query/keys [where]}]
  (assert
   (and
    (some? kind)
    (or (coll? where) (nil? where))))
  (let [clause-fn (compile-clauses {::entity/kind kind} where)
        entity-fn (query/compile-entities kind)
        collection nil #_(utils/coll-id-of-kind kind)]
    (fn
      [a
       {:keys [db]
        :as context}]
      (->
       (clause-fn a (merge context {::query/query (f/coll db collection)}))
       (as-> $
           (assoc
            $
            ::query/docs
            (f/pullv (::query/query $))))
       entity-fn))))

(defn execute
  [a
   context
   qfn]
  (qfn a context))

(defn q
  [a
   context
   query]
  (let [qfn (compile query)]
    (execute a context qfn)))

(defn q-first
  [a context query]
  (->
   (q a context query)
   first))
