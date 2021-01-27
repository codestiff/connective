(ns connective.google.cloud.firestore
  (:require
   [clojure.pprint :as pp]
   [clojure.reflect :as reflect])
  (:import
   (com.google.cloud Timestamp)
   (com.google.cloud.firestore Precondition)))

(def pre-ctor
  (let [decl (into-array java.lang.Class [Boolean Timestamp])]
    (doto (.getDeclaredConstructor Precondition decl)
      (.setAccessible true))))


(defn mk-precondition
  [{:keys [^Boolean exists
           ^Timestamp update-time]}]
  (.newInstance
   pre-ctor
   (into-array Object [exists update-time])))


(comment

  (pp/pprint (reflect/type-reflect Precondition))
  (Precondition/exists true)
  (def decl
    )
  (def ctor
    (.getDeclaredConstructor Precondition decl))

  (.setAccessible ctor true)
  (.newInstance ctor (into-array Object [true nil]))

  (.getConstructor Precondition decl)

  (.newInstance
   (object-array [0]))
  )
