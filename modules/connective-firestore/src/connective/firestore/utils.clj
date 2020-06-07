(ns connective.firestore.utils
  (:require
   [ring.util.codec :as codec]
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
  [context entity doc]
  (let [schema (entity/schema-of-entity context entity)
        relations (entity/relationships-of-schema schema)
        rels (reduce
              (fn
                [r* [k _]]
                (assoc r* k ::entity/pending))
              {}
              relations)
        attrs (reduce
               (fn
                 [e [k v]]
                 (assoc e (keyword k) v))
               {} doc)]
    (->
     entity
     (assoc ::entity/attributes attrs)
     (assoc ::entity/relationships rels))))
