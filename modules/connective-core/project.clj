(defproject codestiff/connective.core "0.1.0-SNAPSHOT"
  :description "An API for each accessing persisted entities and the respective relationships."
  :url "https://github.com/codestiff/connective"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/test.check "1.0.0"]
                 [com.gfredericks/test.chuck "0.2.10"]]

  :aliases {"full" ["do"
                    ["clean"]
                    ["ancient"]
                    ["test"]]}

  :codox {:output-path "codox"
          :metadata {:doc/format :markdown}
          :source-uri "https://github.com/codestiff/connective/blob/{version}/{filepath}#L{line}"}

  :plugins [[lein-codox "0.10.7"]
            [lein-ancient "0.6.15"]]
  :repl-options {:init-ns connective.core})
