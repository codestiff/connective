(ns connective.core
  (:require
   [connective.adapter :as adapter]
   [connective.entity :as entity]))

#_(defprotocol ConnectiveValidation
  "a connective validator"
  (validator
    []
    "returns a validator which can be used to validate entities")
  )

(defmacro defn-of-adaptor
  [sym]
  `(defn ~sym
     [~'adapter ~'context ~'arg]
     (~(symbol "adapter" (str sym)) ~'adapter ~'context ~'arg)))

(defn-of-adaptor related-query)
(defn-of-adaptor reference-query)
(defn-of-adaptor execute-query)
(defn-of-adaptor init-entity)
(defn-of-adaptor write-entity)
(defn-of-adaptor read-entity)
(defn-of-adaptor delete-entity)
(defn-of-adaptor reference-value)

(defn kind-of-definition
  [{::entity/keys [kind]}]
  (assert (some? kind))
  kind)

(defn compile-schema
  [coll]
  (reduce
   (fn
     [cp definition]
     (let [k (kind-of-definition definition)]
       (assert (not (contains? cp k)))
       (assoc cp k definition)))
   {}
   coll))

(def attributes entity/attributes-of-entity)
(def context entity/context-of-entity)
(def ident entity/ident-of-entity)

(comment
  (macroexpand-1 '(defn-of-adaptor related-query))
  )
