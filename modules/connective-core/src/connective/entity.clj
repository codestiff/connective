(ns connective.entity
  (:require
   [connective.validator :as validator]
   [connective.adapter :as adapter]))

(defn schema-of-kind
  [schema kind]
  (if (contains? schema kind)
    (get schema kind)
    (throw
     (ex-info
      "schema for kind not found"
      {:schema schema
       :kind kind}))))

(defn relationships-of-entity
  [{::keys [relationships]}]
  relationships)

(defn assoc-relationships
  [entity relationships]
  (assoc entity ::relationships relationships))

(defn attributes-of-entity
  [{::keys [attributes]}]
  (assert (some? attributes))
  attributes)

(defn assoc-attributes
  [entity attrs]
  (assoc entity ::attributes attrs))

(defn context-of-entity
  [{::keys [context]}]
  context)

(defn assoc-context
  [entity context]
  (assoc entity ::context context))

(defn merge-context
  [entity context]
  (update entity ::context merge context))

(defn assoc-persisted-value
  [entity]
  (merge-context
   entity
   {::persisted-value entity}))

(defn ident-of-entity
  [{::keys [ident]}]
  ident)

(defn ident-of-entity-pending?
  [entity]
  (= ::pending (ident-of-entity entity)))

(defn kind-of-entity
  [{::keys [kind]}]
  (assert (some? kind))
  kind)

(defn id-of-ident
  [{::keys [id]}]
  (assert (some? id))
  id)

(defn kind-of-ident
  [{::keys [kind]}]
  (assert (some? kind))
  kind)

(defn entity-schema
  [{::keys [entity-schema]}]
  (assert (some? entity-schema))
  entity-schema)

(defn persisted-value-of-entity
  [entity]
  (get (context-of-entity entity) ::persisted-value))

(defn schema-of-entity
  [{::keys [schema]
    :as context}
   entity]
  (assert (some? schema))
  (schema-of-kind schema (kind-of-entity entity)))

(defn assoc-ident
  [{::keys [ident]
    :as entity}
   {::keys [id-fn]
    :as schema}]
  (let [attrs (attributes-of-entity entity)
        kind (kind-of-entity entity)
        ident (if (nil? id-fn)
                ::pending
                {::kind kind
                 ::id (id-fn attrs)})]
    (assoc
     entity
     ::kind kind
     ::ident ident)))

(defn entity-schema-contains-relationship?
  [{::keys [relationships] :as schema}
   rel-key]
  (contains? relationships rel-key))

(defn relation-of-entity-schema
  [{::keys [relationships]
    :as e}
   relationship-key]
  (assert (contains? relationships relationship-key))
  (get relationships relationship-key))

(defn relation-of-entity
  [context
   entity
   relationship-key]
  (->
   (schema-of-entity context entity)
   (relation-of-entity-schema relationship-key)))

(defn assoc-reference-attributes-of-relationships
  "Takes a context (db, schema)
  the schema for entity,
  and the entity
  and returns the entity with updated attributes
  in order to propertly relate the entity and it's
  relationships"
  [entity
   connective
   {::keys [entity-schema]
    :as context}]
  ;; this should only overwrite
  ;; relationships that have been added
  ;; otherwise do not change the relationship
  ;; attributes

  ;; we may need 2 versions of this function
  ;; when when before a write
  ;; and the other when first loading...
  ;; maybe if the context includes the
  ;; the parent entity and it's unknown id
  (reduce
   (fn [entity* rel-key]
     (assert
      (entity-schema-contains-relationship? entity-schema rel-key))

     (let [[rel-type {::keys [kind ref-attribute]}] (relation-of-entity-schema entity-schema rel-key)]
       (if (= rel-type ::reference)
         (let [related-entity (get-in entity* [::relationships rel-key])]
           (if (nil? related-entity)
             (assoc-in entity* [::attributes ref-attribute] nil)
             (if (ident-of-entity-pending? related-entity)
               (assoc-in entity* [::attributes ref-attribute] ::pending)
               (let [ref-val (adapter/reference-value connective context related-entity)]
                 (assoc-in entity* [::attributes ref-attribute] ref-val)))))
         entity*)))
   entity
   ;; this is probably a bug we need remove
   ;; references that are part of the
   ;; relationship schema, even if the
   ;; relationship is not set
   (keys (relationships-of-entity entity))))

(defn validate-entity
  [entity
   connective
   context]
  (let [v (adapter/validator connective)]
    (cond
      (nil? v) entity
      (validator/validate v context entity) entity
      :else (throw
             (ex-info
              "Failed Validation"
              (validator/explain v context entity))))))

(defn prepare-entity
  [connective
   context
   entity]
  (let [entity-schema (schema-of-entity context entity)
        context (assoc context ::entity-schema entity-schema)
        entity (->
                (merge {::attributes {}} entity)
                (assoc-reference-attributes-of-relationships connective context)
                (validate-entity connective context))]

    ;; maybe assoc ident should add the ident if id-fn exists
    ;; otherwise set it to ::pending
    ;; probably a bad idea, but let's keep for now
    ;; should probably change the name
    (assoc-ident entity entity-schema)))

(defn init
  [connective
   context
   entity]
  (->
   (prepare-entity connective context entity)
   (assoc-context {})))
