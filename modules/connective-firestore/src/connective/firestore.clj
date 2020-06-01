(ns connective.firestore
  (:require
   [firestore-clj.core :as f]
   [connective.adapter :as adapter]
   [connective.entity :as entity]))

(deftype FirestoreAdapter
    [config]

  adapter/IAdapter

  (init-entity
    [a context entity]
    (entity/init a context entity))

  (related-query
    [_ context params]
    (prn context)
    (prn params))

  (reference-query
    [_ context params]
    (prn context)
    (prn params))

  (execute-query
    [_ context query]
    (prn context)
    (prn query)
    )

  (write-entity
    [_ context entity]
    (prn context)
    (prn entity))

  (read-entity
    [_ context entity]
    (prn context)
    (prn entity))

  (delete-entity
    [_ context entity]
    (prn context)
    (prn entity)))

(def fs (FirestoreAdapter. nil))

(comment

  (core/write-entity fs {} {})
  )
