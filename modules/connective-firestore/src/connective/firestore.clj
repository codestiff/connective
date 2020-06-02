(ns connective.firestore
  (:require
   [ring.util.codec :as codec]
   [firestore-clj.core :as f]
   [connective.adapter :as adapter]
   [connective.entity :as entity]))

(defn coll-id-of-ident
  [{::entity/keys [kind]}]
  (assert (some? kind))
  (->
   kind
   pr-str
   codec/url-encode))

(defn doc-id
  [{::entity/keys [ident] :as entity}]
  (assert (some? ident))
  (let [c-id (coll-id-of-ident ident)
        ident (entity/id-of-ident ident)
        _ (assert (some? ident))
        d-id (->
              ident
              pr-str
              codec/url-encode)]
    (str c-id "/" d-id)))

(defn doc-attributes
  [entity]
  (reduce
   (fn
     [attrs [k v]]
     (assoc attrs (name k) v))
   {}
   (entity/attributes-of-entity entity)))

(defn assoc-entity-attributes
  [entity doc]
  (let [attrs (reduce
               (fn
                 [e [k v]]
                 (assoc e (keyword k) v))
               {} doc)]
    (assoc entity ::entity/attributes attrs)))

(deftype FirestoreAdapter
    [config]

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
          doc-id (doc-id entity)
          doc-attrs (doc-attributes entity)]
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
    (let [base-entity {::entity/ident ident
                       ::entity/kind kind}
          doc-id (doc-id base-entity)
          doc (->
               (f/doc conn doc-id)
               (f/pull))
          entity (assoc-entity-attributes base-entity doc)
          entity (entity/init
                  a
                  context
                  entity)]
      (entity/assoc-persisted-value entity)))

  (delete-entity
    [_ context entity]
    (prn context)
    (prn entity))

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

  )

(def fs (FirestoreAdapter. nil))

(comment

  (core/write-entity fs {} {})
  )
