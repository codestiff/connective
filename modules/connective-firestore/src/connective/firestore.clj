(ns connective.firestore
  (:require
   [firestore-clj.core :as f]
   [connective.core :as core]))

(deftype FirestoreSystem [config]
    core/ConnectiveSystem

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

(def fs (FirestoreSystem. nil))

(comment

  (core/write-entity fs {} {})
  )
