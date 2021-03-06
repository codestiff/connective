(defproject codestiff/connective.firestore "0.1.10-SNAPSHOT"
  :description "A connective adaptor for firestore"
  :url "https://github.com/codestiff/connective"

  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :managed-dependencies [[io.grpc/grpc-netty-shaded "1.35.0"]
                         [io.grpc/grpc-core "1.35.0"]
                         [io.grpc/grpc-api "1.35.0"]
                         [com.google.errorprone/error_prone_annotations "2.5.1"]]

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [codestiff/connective.core "0.1.8"]
                 [polvo/firestore-clj "1.2.1"]
                 [ring/ring-codec "1.1.2"]]
  :profiles {:dev {:dependencies [[codestiff/connective.malli "0.1.8"]]}}
  :repl-options {:init-ns connective.firestore})
