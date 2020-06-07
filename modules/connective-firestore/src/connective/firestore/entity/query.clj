(ns connective.firestore.entity.query
  (:require
   [connective.entity :as entity]
   [connective.firestore.query :as query]
   [connective.firestore.utils :as utils]
   [firestore-clj.core :as f]))

(defmulti compile-clause
  (fn [_ [op & xs]] op))

(defmethod compile-clause :default
  [context params]
  (query/compile-clause context params))

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
        collection (utils/coll-id-of-ident {::entity/kind kind})]
    (fn
      [a
       {::entity/keys [conn]
        :as context}]
      (->
       (clause-fn a (merge context {::query/query (f/coll conn collection)}))
       (as-> $
           (assoc
            $
            ::query/docs
            (f/pullv (::query/query $)))
         (entity-fn a $))))))

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
