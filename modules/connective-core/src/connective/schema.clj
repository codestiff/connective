(ns connective.schema
  (:require
   [connective.entity :as entity]))

(defn kind-of-definition
  [{::entity/keys [kind]}]
  (assert (some? kind))
  kind)

(defn default-ref-attribute
  [kind]
  (let [kind-str (name kind)]
    (keyword kind-str "-ref")))

(defn compile-relation
  [definition
   [rel-type {::entity/keys [kind]
              :as args}]]
  (assert (some? kind))
  (let [args (merge
              {::entity/ref-attribute (default-ref-attribute kind)}
              args)]
    [rel-type args]))

(defn compile-relationships
  [{::entity/keys [relationships]
    :as definition}]
  (let [compiled-rels (reduce
                       (fn
                         [rels* [k relation]]
                         (assoc rels* k (compile-relation definition relation)))
                       {}
                       relationships)]
    (assoc definition ::entity/relationships compiled-rels)))

(defn compile-definition
  [{::entity/keys [kind
                   attributes]
    :as definition}]
  (assert
   (and
    (some? kind)
    (some? attributes)))
  (cond-> definition
    (contains? definition ::entity/relationships) compile-relationships))

(defn compile
  [coll]
  (reduce
   (fn
     [cp definition]
     (let [k (kind-of-definition definition)
           definition (compile-definition definition)]
       (assert (not (contains? cp k)))
       (assoc cp k definition)))
   {}
   coll))
