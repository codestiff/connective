(ns connective.validator)

(defprotocol IValidator
  "a connective validator"

  (validate
    [_ context entity]
    "return whether the entity is valid or not")

  (explain
    [_ context entity]
    "returns a map representing the parts of the entity
     which do not pass validation"))
