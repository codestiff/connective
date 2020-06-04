(ns connective.malli
  (:require
   [malli.core :as m]
   [connective.validator :as validator]
   [connective.entity :as entity]
   [connective.core :as core]))

(deftype MalliValidator
    []

  validator/IValidator

  (validate
    [_ context entity]
    (let [attrs (core/attributes entity)
          {::entity/keys [attributes]} (entity/entity-schema context)]
      (if (nil? attributes)
        true
        (m/validate attributes attrs))))

  (explain
    [_ context entity]
    (let [attrs (core/attributes entity)
          {::entity/keys [attributes]} (entity/entity-schema context)]
      (assert (some? attributes))
      (m/explain attributes attrs))))

(defn validator
  []
  (MalliValidator.))
