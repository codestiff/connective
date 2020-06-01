(ns connective.adapter)

(defprotocol IAdapter
  "a connective adapter"

  (related-query
    [_ context params]
    "create a query which will be passed to execute
     query in order to fetch backing entities data")

  (reference-query
    [_ context params]
    "create a query which will be passed to execute
     query in order to fetch backing entity data")

  (execute-query
    [_ context query]
    "execute a query which return a collection of entity data")

  (init-entity
    [_ context entity]
    "initialize a new entity")

  (write-entity
    [_ context entity]
    "writes an entity to the backend system")

  (read-entity
    [_ context entity]
    "read an entity from the backend system")

  (delete-entity
    [_ context entity]
    "deletes an entity from the backend system")

  (reference-value
    [_ context entity]
    "given a context, params of the related entity")

  )
