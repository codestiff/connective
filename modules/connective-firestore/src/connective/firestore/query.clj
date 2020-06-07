(ns connective.firestore.query
  (:require
   [connective.entity :as entity]
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
  (assoc context ::query (f/filter-in query (name attr) value))
  )

(defn- compile-clauses
  [where]
  (fn
    [context]
    (reduce
     compile-clause
     context
     where)))

(defn compile-entity
  [kind]
  (fn
    [{:keys [schema]
      ::keys [docs]}]
    (assert (some? schema))
    (let [entity-schema (utils/schema-of-kind
                         schema
                         kind)]
      (for [doc docs]
        (entity/entify-doc
         entity-schema
         {::entity/exists? true}
         doc)))))

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
                    (fn [x & xs] x)
                    (compile-entity into))]
    (fn
      [{:keys [db]
        :as context}]
      (->
       {::query (f/coll db collection)}
       (merge context)
       clause-fn
       (as-> $
           (assoc
            $
            ::docs
            (f/pullv (::query $))))
       entity-fn))))

(defn q
  [context
   query]
  (let [qfn (compile query)]
    (->
     context
     qfn
     ::docs)))
