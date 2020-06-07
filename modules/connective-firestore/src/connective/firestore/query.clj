(ns connective.firestore.query
  (:require
   [connective.firestore.utils :as utils]
   [connective.entity :as entity]
   [connective.core :as core]
   [firestore-clj.core :as f]))

(defmulti compile-clause
  (fn [_ [op & xs]] op))

(defmethod compile-clause :=
  [{::keys [query]
    :as context} [_ attr value]]
  (assert
   (and (some? attr)
        (some? value)))
  (assoc context ::query (f/filter= query (name attr) value)))

(defmethod compile-clause :in
  [{::keys [query]
    :as context} [_ attr value]]
  (assert
   (and (some? attr)
        (some? value)))
  (assoc context ::query (f/filter-in query (name attr) value)))

(defn- compile-clauses
  [where]
  (fn
    [a
     ctx]
    (reduce
     compile-clause
     ctx
     where)))

(defn compile-entities
  [kind]
  (fn
    [a
     {::keys [docs]
      :as ctx}]
    (assert (some? kind))
    (let [entities (for [doc-data docs]
                     (let [base-entity {::entity/kind kind}
                           entity (utils/assoc-entity-attributes
                                   base-entity
                                   doc-data)
                           entity (core/init-entity
                                   a
                                   ctx
                                   entity)]
                       (entity/assoc-persisted-value entity))) ]
      entities)))

(defn compile
  [{::keys [find
            collection
            where
            into]}]
  (assert
   (and (= find ::doc)
        (some? collection)
        (or (coll? where) (nil? where))))
  (let [clause-fn (compile-clauses where)
        entity-fn (if (nil? into)
                    (fn [a ctx & xs] ctx)
                    (compile-entities into))]
    (fn
      [a
       {:keys [db]
        :as context}]
      (->
       (clause-fn
        a
        (merge context {::query (f/coll db collection)}))
       (as-> $
           (assoc
            $
            ::docs
            (f/pullv (::query $))))
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
