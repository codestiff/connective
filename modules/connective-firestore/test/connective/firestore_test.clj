(ns connective.firestore-test
  (:require
   [clojure.test :refer :all]
   [connective.firestore :refer :all]
   [connective.core :as core]
   [connective.entity :as entity]
   [firestore-clj.core :as f]))

(defonce db
  (f/emulator-client "project-local-1" "localhost:8080"))

(def schema
  [
   {::entity/kind ::items
    ::entity/id-fn :sku
    ::entity/attributes [:map
                         [:sku string?]
                         [:name string?]
                         [:price double?]]
    ::entity/relationships {:keywords [::entity/many {::entity/kind ::item-search-keywords}]}}

   {::entity/kind ::item-search-keywords
    ::entity/primary-key (juxt :keyword (comp f/id :item-ref))
    ::entity/attributes [:map
                         [:keyword string?]
                         [:item-ref some?]]
    ::entity/relationships {:item [::entity/reference {::entity/kind ::items
                                                       ::entity/ref-attribute :item-ref}]}}

   {::entity/kind ::shopping-cart-items
    ::entity/primary-key (juxt
                          (comp f/id :shopping-cart-ref)
                          (comp f/id :item-ref))
    ::entity/attributes [:map
                         [:shopping-cart-ref some?]
                         [:item-ref some?]]
    ::entity/relationships {:item [::entity/reference {::entity/kind ::items
                                                       ::entity/ref-attribute :item-ref}]
                            :shopping-cart [::entity/reference {::entity/kind ::shopping-carts
                                                                ::entity/ref-attribute :shopping-cart-ref}]}}

   {::entity/kind ::shopping-carts
    ::entity/primary-key :user-id
    ::entity/attributes [:map
                         [:user-id string?]]
    ::entity/relationships {:shopping-cart-items [::entity/many {::entity/kind ::shopping-cart-items}]}}

   {::entity/kind ::payment-methods
    ::entity/primary-key (juxt :user-id :payment-id)
    ::entity/attributes [:map
                         [:user-id string?]
                         [:payment-id string?]
                         [:credit-card-number string?]]
    ::entity/relationships {:orders [::entity/many {::entity/kind ::orders}]}}

   {::entity/kind ::orders
    ::entity/primary-key :order-id
    ::entity/attributes [:map
                         [:order-id string?]
                         [:user-id string?]
                         [:payment-method-ref some?]
                         [:item-count int?]
                         [:total number?]]
    ::entity/relationships {:payment-method [::entity/reference {::entity/kind ::payment-methods
                                                                 ::entity/ref-attribute :payment-method-ref}]}}
   ])

(def c-schema
  (core/compile-schema schema))

(def context
  {::entity/schema c-schema
   ::entity/conn db})

#_(def fs (atom (FirestoreAdapter. nil)))

#_(def ball-item
  (core/init-entity
   fs
   context
   {::entity/kind ::items
    ::entity/attributes {:sku "ff-0012"
                         :name "kitten"
                         :description "So many wonder kittens to play with. Try them all."
                         :price 128.99}}))

(deftest a-basic-init-entity-test
  (testing "an example init test"
    (let [attrs {:sku "ff-0012"
                 :name "kitten"
                 :description "So many wonder kittens to play with. Try them all."
                 :price 128.99}

          entity (core/init-entity
                  fs
                  context
                  {::entity/kind ::items
                   ::entity/attributes attrs})

          expected-ident {::entity/kind ::items
                          ::entity/id (:sku attrs)}]

      (is (= attrs (core/attributes entity)))

      (is (= {} (core/context entity)))

      (is (nil? (entity/persisted-value-of-entity entity)))

      (is (= expected-ident (core/ident entity)))
      ))

  (testing "validation fails"
    (let [attrs {:sku "ff-0012"
                 :name 2
                 :description "So many wonder kittens to play with. Try them all."
                 :price "hello"}]

      (is (thrown?
           clojure.lang.ExceptionInfo
           (core/init-entity
            fs
            context
            {::entity/kind ::items
             ::entity/attributes attrs}))))
    ))

(deftest a-basic-write-entity-test
  (testing "an example write entity test"
    (let [attrs {:sku "ff-0012"
                 :name "kitten"
                 :description "So many wonder kittens to play with. Try them all."
                 :price 128.99}

          original-entity (core/init-entity
                           fs
                           context
                           {::entity/kind ::items
                            ::entity/attributes attrs})

          expected-ident {::entity/kind ::items
                          ::entity/id (:sku attrs)}

          written-entity (core/write-entity
                          fs
                          context
                          original-entity)]

      (is (= attrs (core/attributes written-entity)))

      (is (contains? (core/context written-entity) ::entity/persisted-value))

      (is (=
           (update written-entity ::entity/context dissoc ::entity/persisted-value)
           (entity/persisted-value-of-entity written-entity)))

      (is (= expected-ident (core/ident written-entity)))

      )))

(deftest a-basic-read-entity-test
  (testing "an example read entity test"
    (let [attrs {:sku "ff-0012"
                 :name "kitten"
                 :description "So many wonder kittens to play with. Try them all."
                 :price 128.99}

          original-entity (core/init-entity
                           fs
                           context
                           {::entity/kind ::items
                            ::entity/attributes attrs})

          written-entity (core/write-entity
                          fs
                          context
                          original-entity)

          ident (entity/ident-of-entity original-entity)

          read-entity (core/read-entity
                       fs
                       context
                       ident)]

      (is (= attrs (core/attributes read-entity)))

      (is (contains? (core/context read-entity) ::entity/persisted-value))

      (is (=
           original-entity
           (entity/persisted-value-of-entity read-entity)))

      (is (= ident (core/ident read-entity)))

      )))

(deftest a-basic-delete-entity-test
  (testing "an example delete entity test"
    (let [attrs {:sku "ff-0012"
                 :name "kitten"
                 :description "So many wonder kittens to play with. Try them all."
                 :price 128.99}

          original-entity (core/init-entity
                           fs
                           context
                           {::entity/kind ::items
                            ::entity/attributes attrs})

          _ (core/write-entity
             fs
             context
             original-entity)

          ident (entity/ident-of-entity original-entity)

          deleted-ident (core/delete-entity
                         fs
                         context
                         ident)]

      (is (nil? (core/read-entity fs context ident))))
    ))

(comment

  )
