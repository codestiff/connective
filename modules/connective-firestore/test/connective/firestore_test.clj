(ns connective.firestore-test
  (:require
   [clojure.test :refer :all]
   [connective.firestore :refer :all]
   [connective.core :as core]
   [connective.entity :as entity]
   [firestore-clj.core :as f]
   [ring.util.codec :as codec]))

(defonce db
  (f/emulator-client "project-local-1" "localhost:8080"))

(defn encode-id
  [v]
  (->
   v
   pr-str
   codec/url-encode))

(defn id-fn-refs
  [& coll]
  (fn
    [attrs]
    (reduce
     (fn
       [id [attr-key ref?]]
       (let [attr (get attrs attr-key)]
         (if ref?
           (cond
             (= attr ::entity/pending) (reduced ::entity/pending)
             :else (conj id (f/id attr)))
           (conj id attr))))
     []
     coll)))

(defn without-rels
  [entity]
  (dissoc entity ::entity/relationships))

(defn without-attrs
  [entity key-coll]
  (reduce
   (fn [e* attr-key]
     (update e* ::entity/attributes dissoc attr-key))
   entity
   key-coll))

(defn without-context
  [entity]
  (dissoc entity ::entity/context))

(defn without
  [entity {:keys [attrs]
           :as opts}]
  (cond-> entity
    (some? attrs) (without-attrs attrs)
    (contains? opts :rels) (without-rels)
    (contains? opts :context) (without-context)))

(defn into-rels
  [entity key-coll]
  (reduce
   (fn [e* rel-key]
     (get (core/relationships e*) rel-key))
   entity
   key-coll))

(def schema
  [
   {::entity/kind ::items
    ::entity/id-fn :sku
    ::entity/attributes [:map
                         [:sku string?]
                         [:name string?]
                         [:price double?]]
    ::entity/relationships {:keywords [::entity/many {::entity/kind ::item-search-keywords
                                                      ::entity/ref-attribute :item-ref}]}}

   {::entity/kind ::item-search-keywords
    ::entity/id-fn (id-fn-refs [:keyword] [:item-ref true])
    ::entity/attributes [:map
                         [:keyword string?]
                         [:item-ref some?]]
    ::entity/relationships {:item [::entity/reference {::entity/kind ::items
                                                       ::entity/ref-attribute :item-ref}]}}

   {::entity/kind ::shopping-cart-items
    ::entity/id-fn (id-fn-refs [:shopping-cart-ref true] [:item-ref true])
    ::entity/attributes [:map
                         [:shopping-cart-ref some?]
                         [:item-ref some?]]
    ::entity/relationships {:item [::entity/reference {::entity/kind ::items
                                                       ::entity/ref-attribute :item-ref}]
                            :shopping-cart [::entity/reference {::entity/kind ::shopping-carts
                                                                ::entity/ref-attribute :shopping-cart-ref}]}}

   {::entity/kind ::shopping-carts
    ::entity/id-fn :user-id
    ::entity/attributes [:map
                         [:user-id string?]]
    ::entity/relationships {:shopping-cart-items [::entity/many {::entity/kind ::shopping-cart-items
                                                                 ::entity/ref-attribute :shopping-cart-ref}]}}

   {::entity/kind ::payment-methods
    ::entity/id-fn (juxt :user-id :payment-id)
    ::entity/attributes [:map
                         [:user-id string?]
                         [:payment-id string?]
                         [:credit-card-number string?]]
    ::entity/relationships {:orders [::entity/many {::entity/kind ::orders
                                                    ::entity/ref-attribute :payment-method-ref}]}}

   {::entity/kind ::orders
    ::entity/id-fn :order-id
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
  (core/compile-schema schema)
  )

(def context
  {::entity/schema c-schema
   ::entity/conn db})

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
           (without written-entity {:context nil} )
           (entity/persisted-value-of-entity written-entity)))

      (is (= expected-ident (core/ident written-entity)))

      ))

  (testing "an example write entity test"
    (let [attrs {:sku "ff-0012"
                 :name "kitten"
                 :description "So many wonder kittens to play with. Try them all."
                 :price 128.99}

          original-entities (core/init-entities
                             fs
                             context
                             [{::entity/kind ::items
                               ::entity/attributes attrs}])

          expected-ident [{::entity/kind ::items
                           ::entity/id (:sku attrs)}]

          written-entities (core/write-entities
                            fs
                            context
                            original-entities)
          written-entity (first written-entities)]

      (is (= attrs (core/attributes written-entity)))

      (is (contains? (core/context written-entity) ::entity/persisted-value))

      ))
  )

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
                       ident)
          ]

      (is (= attrs (core/attributes read-entity)))

      (is (contains? (core/context read-entity) ::entity/persisted-value))

      (is (=
           (without original-entity {:context nil})
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

(defmacro with-example-entity-tree
  [& body]
  `(let [~'keyword-entities ~'(fn
                                [paragraph]
                                (let [keywords (->>
                                                (clojure.string/split paragraph #"\s+")
                                                (concat [])
                                                (map #(clojure.string/replace % #"\W" "")))]
                                  (for [kw keywords]
                                    {::entity/kind ::item-search-keywords
                                     ::entity/attributes {:keyword kw}})))

         ~'item-1-description "So many wonder kittens to play with. Try them all."
         ~'item-2-description "Cold cereal. Goes great with milk!"

         ~'item-1 {::entity/kind ::items
                   ::entity/attributes {:sku "ff-0012"
                                        :name "kitten"
                                        :description ~'item-1-description
                                        :price 128.99}

                   ::entity/relationships {:keywords ~'(keyword-entities item-1-description)}}

         ~'item-2 {::entity/kind ::items
                   ::entity/attributes {:sku "gd-4921"
                                        :name "cereal"
                                        :description ~'item-2-description
                                        :price 4.78}

                   ::entity/relationships {:keywords ~'(keyword-entities item-2-description)}}

         ~'shopping-cart-item-1 {::entity/kind ::shopping-cart-items
                                 ::entity/relationships {:item ~'item-1}}

         ~'shopping-cart-item-2 {::entity/kind ::shopping-cart-items
                                 ::entity/relationships {:item ~'item-2}}

         ~'shopping-cart {::entity/kind ::shopping-carts
                          ::entity/attributes {:user-id "user-1"}
                          ::entity/relationships {:shopping-cart-items [~'shopping-cart-item-1
                                                                        ~'shopping-cart-item-2]}}
         ]
     ~@body))

(deftest an-example-init-relationships-test
  (testing "a example of reading relationships"
    (with-example-entity-tree
      (let [s-cart (core/init-rels fs context shopping-cart)]
        (is
         (= {::entity/kind ::shopping-carts
             ::entity/ident {::entity/kind ::shopping-carts
                             ::entity/id "user-1"}
             ::entity/context {}
             ::entity/attributes {:user-id "user-1"}} (without-rels s-cart)))

        (let [expected [{::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "ff-0012"])}
                         ::entity/context {}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}

                        {::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "gd-4921"])}
                         ::entity/context {}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}]

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      (as-> $
                          (map
                           #(without % {:attrs [:item-ref] :rels nil})
                           $)))]
          (is (= expected actual)))

        (let [expected {::entity/kind ::items
                        ::entity/ident {::entity/kind ::items
                                        ::entity/id "ff-0012"}
                        ::entity/context {}
                        ::entity/attributes {:sku "ff-0012"
                                             :name "kitten"
                                             :description item-1-description
                                             :price 128.99}}

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item])
                      (without {:rels nil}))]

          (is (= expected actual)))

        (let [expected (for [e (keyword-entities item-1-description)]
                         (->
                          e
                          (assoc
                           ::entity/ident {::entity/kind ::item-search-keywords
                                           ::entity/id [(-> e ::entity/attributes :keyword) (encode-id "ff-0012")]}
                           ::entity/context {})
                          (update ::entity/attributes assoc :item-ref ::entity/parent)))

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item :keywords])
                      (as-> $
                          (map
                           #(without % {:rels nil})
                           $)
                        ))]
          (is (= expected actual))))
        )))

(deftest an-example-write-relationships-test
  (testing "a example of reading relationships"
    (with-example-entity-tree
      (let [s-cart (core/write-rels fs context shopping-cart)]
        (is
         (= {::entity/kind ::shopping-carts
             ::entity/ident {::entity/kind ::shopping-carts
                             ::entity/id "user-1"}
             ::entity/attributes {:user-id "user-1"}}

             (without s-cart {:rels nil :context nil})))

        (is
         (=
          (without s-cart {:rels nil :context nil})
          (entity/persisted-value-of-entity s-cart)))


        (let [expected [{::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "ff-0012"])}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}

                        {::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "gd-4921"])}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}]

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      (as-> $
                          (map
                           #(without % {:attrs [:item-ref] :rels nil})
                           $)))]
          (is (= expected (map #(without % {:context nil}) actual)))

          (doseq [a actual]
            (let [{::entity/keys [attributes]} (entity/persisted-value-of-entity a)]
              (doseq [k [:item-ref :shopping-cart-ref]]
                (is (some? (k attributes)))))))

        (let [expected {::entity/kind ::items
                        ::entity/ident {::entity/kind ::items
                                        ::entity/id "ff-0012"}
                        ::entity/attributes {:sku "ff-0012"
                                             :name "kitten"
                                             :description item-1-description
                                             :price 128.99}}

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item])
                      (without {:rels nil}))]

          (is (= expected (without actual {:context nil})))
          (is (= expected (entity/persisted-value-of-entity actual))))

        (let [expected (for [e (keyword-entities item-1-description)]
                         (->
                          e
                          (assoc
                           ::entity/ident {::entity/kind ::item-search-keywords
                                           ::entity/id [(-> e ::entity/attributes :keyword) (encode-id "ff-0012")]}
                           )
                          (update ::entity/attributes assoc :item-ref ::entity/parent)))

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item :keywords])
                      (as-> $
                          (map
                           #(without % {:rels nil})
                           $)
                        ))]

          (is (= expected (map #(without % {:context nil}) actual)))

          (doseq [a actual]
            (let [{::entity/keys [attributes]} (entity/persisted-value-of-entity a)]
              (doseq [k [:item-ref]]
                (is (some? (k attributes))))))
          )
        )
      )))

(deftest an-example-read-relationships-test
  (testing "a example of reading relationships"
    (with-example-entity-tree
      (let [written-s-cart (core/write-rels fs context shopping-cart)
            ident (entity/ident-of-entity written-s-cart)
            s-cart (core/read-rels
                    fs
                    context
                    (assoc
                     ident
                     ::entity/relationships
                     {:shopping-cart-items
                      {::entity/relationships
                       {:item
                        {::entity/relationships
                         {:keywords nil}}}}}))]

        (is
         (= {::entity/kind ::shopping-carts
             ::entity/ident {::entity/kind ::shopping-carts
                             ::entity/id "user-1"}
             ::entity/attributes {:user-id "user-1"}}

             (without s-cart {:rels nil :context nil})))

        (is
         (=
          (without s-cart {:rels nil :context nil})
          (entity/persisted-value-of-entity s-cart)))


        (let [expected [{::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "ff-0012"])}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}

                        {::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "gd-4921"])}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}]

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      (as-> $
                          (map
                           #(without % {:attrs [:item-ref] :rels nil})
                           $)))]
          (is (= expected (map #(without % {:context nil}) actual)))

          (doseq [a actual]
            (let [{::entity/keys [attributes]} (entity/persisted-value-of-entity a)]
              (doseq [k [:item-ref :shopping-cart-ref]]
                (is (some? (k attributes)))))))

        (let [expected {::entity/kind ::items
                        ::entity/ident {::entity/kind ::items
                                        ::entity/id "ff-0012"}
                        ::entity/attributes {:sku "ff-0012"
                                             :name "kitten"
                                             :description item-1-description
                                             :price 128.99}}

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item])
                      (without {:rels nil}))]

          (is (= expected (without actual {:context nil})))
          (is (= expected (entity/persisted-value-of-entity actual))))

        (let [expected (for [e (keyword-entities item-1-description)]
                         (->
                          e
                          (assoc
                           ::entity/ident {::entity/kind ::item-search-keywords
                                           ::entity/id [(-> e ::entity/attributes :keyword) (encode-id "ff-0012")]}
                           )
                          (update ::entity/attributes assoc :item-ref ::entity/parent)))

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item :keywords])
                      (as-> $
                          (map
                           #(without % {:rels nil})
                           $)
                        ))]

          (is (= (frequencies expected) (frequencies (map #(without % {:context nil}) actual))))

          (doseq [a actual]
            (let [{::entity/keys [attributes]} (entity/persisted-value-of-entity a)]
              (doseq [k [:item-ref]]
                (is (some? (k attributes))))))
          )
        )
      )))

(deftest an-example-delete-relationships-test
  (testing "a example of deleting relationships"
    (with-example-entity-tree
      (let [written-s-cart (core/write-rels fs context shopping-cart)
            ident (entity/ident-of-entity written-s-cart)
            s-cart (core/delete-rels
                    fs
                    context
                    (assoc
                     written-s-cart
                     ::core/delete-all
                     {:shopping-cart-items
                      {::entity/relationships
                       {:item
                        {::entity/relationships
                         {:keywords nil}}}}}))]

        (is
         (= {::entity/kind ::shopping-carts
             ::entity/ident {::entity/kind ::shopping-carts
                             ::entity/id "user-1"}
             ::entity/attributes {:user-id "user-1"}}

             (without s-cart {:rels nil :context nil})))

        #_(is
         (=
          (without s-cart {:rels nil :context nil})
          (entity/persisted-value-of-entity s-cart)))


        #_(let [expected [{::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "ff-0012"])}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}

                        {::entity/kind ::shopping-cart-items
                         ::entity/ident {::entity/kind ::shopping-cart-items
                                         ::entity/id (mapv encode-id ["user-1" "gd-4921"])}
                         ::entity/attributes {:shopping-cart-ref ::entity/parent}}]

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      (as-> $
                          (map
                           #(without % {:attrs [:item-ref] :rels nil})
                           $)))]
          (is (= expected (map #(without % {:context nil}) actual)))

          (doseq [a actual]
            (let [{::entity/keys [attributes]} (entity/persisted-value-of-entity a)]
              (doseq [k [:item-ref :shopping-cart-ref]]
                (is (some? (k attributes)))))))

        #_(let [expected {::entity/kind ::items
                        ::entity/ident {::entity/kind ::items
                                        ::entity/id "ff-0012"}
                        ::entity/attributes {:sku "ff-0012"
                                             :name "kitten"
                                             :description item-1-description
                                             :price 128.99}}

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item])
                      (without {:rels nil}))]

          (is (= expected (without actual {:context nil})))
          (is (= expected (entity/persisted-value-of-entity actual))))

        #_(let [expected (for [e (keyword-entities item-1-description)]
                         (->
                          e
                          (assoc
                           ::entity/ident {::entity/kind ::item-search-keywords
                                           ::entity/id [(-> e ::entity/attributes :keyword) (encode-id "ff-0012")]}
                           )
                          (update ::entity/attributes assoc :item-ref ::entity/parent)))

              actual (->
                      s-cart
                      (into-rels [:shopping-cart-items])
                      first
                      (into-rels [:item :keywords])
                      (as-> $
                          (map
                           #(without % {:rels nil})
                           $)
                        ))]

          (is (= (frequencies expected) (frequencies (map #(without % {:context nil}) actual))))

          (doseq [a actual]
            (let [{::entity/keys [attributes]} (entity/persisted-value-of-entity a)]
              (doseq [k [:item-ref]]
                (is (some? (k attributes))))))
          )
        )
      )))

(comment

  )
