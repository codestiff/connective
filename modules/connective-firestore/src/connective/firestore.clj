(ns connective.firestore
  (:require
   [firestore-clj.core :as f]
   [connective.malli :as malli]
   [connective.validator :as validator]
   [connective.adapter :as adapter]
   [connective.entity :as entity]
   [connective.firestore.entity.query :as e.query]
   [connective.firestore.utils :as utils]
   [connective.core :as core]))

(deftype FirestoreAdapter
    [params]

  adapter/IAdapter

  (init-entity
    [a context entity]
    (entity/init a context entity))

  (write-entity
    [a
     {::entity/keys [conn]
      :as context}
     entity]
    (let [entity (entity/init a context entity)
          doc-id (utils/doc-id entity)
          doc-attrs (utils/doc-attributes entity)]
      (->
       (f/doc conn doc-id)
       (f/set! doc-attrs))
      (entity/assoc-persisted-value entity)))

  (read-entity
    [a
     {::entity/keys [conn]
      :as context}
     {::entity/keys [kind]
      :as ident}]
    (assert (some? kind))
    (let [
          base-entity {::entity/ident ident
                       ::entity/kind kind}
          doc-id (utils/doc-id base-entity)
          snapshot (->
                    (f/doc conn doc-id)
                    (f/doc-snap))]
      (if (f/exists? snapshot)
        (let [data (f/ds->plain snapshot)
              entity (utils/assoc-entity-attributes base-entity data)
              entity (entity/init
                      a
                      context
                      entity)]
          (entity/assoc-persisted-value entity))
        nil)))

  (reference-value
    [_
     {::entity/keys [conn]
      :as context}
     entity]
    (assert (some? entity))
    (let [doc-id (utils/doc-id entity)]
      (f/doc conn doc-id)))

  (delete-entity
    [_
     {::entity/keys [conn]
      :as context}
     {::entity/keys [kind]
      :as ident}]
    (assert (some? kind))
    (let [base-entity {::entity/ident ident
                       ::entity/kind kind}
          doc-id (utils/doc-id base-entity)
          doc (->
               (f/doc conn doc-id)
               (f/delete!))]
      ident))

  (related-query
    [_
     context
     {::entity/keys [ref-attribute
                     kind
                     ref-value]}]
    (prn params)
    [])

  (reference-query
    [_
     context
     {::entity/keys [ref-attribute
                     kind
                     ref-value]}]
    (prn params)
    nil)

  (execute-query
    [a context query]
    (e.query/execute a context query))

  (validator
    [_]
    (get params ::validator)))

(def fs
  (FirestoreAdapter.
   {::validator (malli/validator)}))

(comment

  (core/write-entity fs {} {})
  )
