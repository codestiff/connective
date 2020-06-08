(defproject codestiff/connective "0.1.1-SNAPSHOT"
  :description "An API for each accessing persisted entities and the respective relationships."
  :url "https://github.com/codestiff/connective"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["sub" "change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["shell" "git" "branch" "-f" "release"]
                  ["shell" "git" "push" "origin" "release" "-f"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["sub" "change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]

  :dependencies [[org.clojure/clojure "1.10.1"]]

  :sub [
        "modules/connective-core"
        "modules/connective-malli"
        "modules/connective-firestore"
        ]

  :aliases {"docs" ["with-profile" "dev,docs" "codox"]}

  :profiles {:docs {:plugins [[lein-codox "0.10.7"]]
                    :dependencies [
                                   [codestiff/connective.core "0.1.0-SNAPSHOT"]
                                   [codestiff/connective.malli "0.1.0-SNAPSHOT"]
                                   [codestiff/connective.firestore "0.1.0-SNAPSHOT"]
                                   ]

                    :codox {:output-path "codox"
                            :metadata {:doc/format :markdown}
                            :source-uri "https://github.com/codestiff/connective/blob/{version}/{filepath}#L{line}"
                            :source-paths [
                                           "modules/connective-core/src"
                                           "modules/connective-malli/src"
                                           "modules/connective-firestore/src"
                                           ]
                            }}}

  :plugins [[lein-sub "0.3.0"]
            [lein-codox "0.10.7"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]])
