(defproject codestiff/connective "0.1.0-SNAPSHOT"
  :description "An API for each accessing persisted entities and the respective relationships."
  :url "https://github.com/codestiff/connective"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :aliases {"docs" ["with-profile" "docs" "codox"]}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :sub [
        "modules/connective-core"
        "modules/connective-firestore"
        ]
  :profiles {:docs {:plugins [[lein-codox "0.10.7"]]
                    :dependencies [[codestiff/connective.core "0.1.0-SNAPSHOT"]
                                   [codestiff/connective.firestore "0.1.0-SNAPSHOT"]]

                    :codox {:output-path "codox"
                            :metadata {:doc/format :markdown}
                            :source-uri "https://github.com/codestiff/connective/blob/{version}/{filepath}#L{line}"
                            :source-paths ["modules/connective-core/src"]}}}

  :plugins [[lein-sub "0.3.0"]
            [lein-codox "0.10.7"]
            [lein-ancient "0.6.15"]])
