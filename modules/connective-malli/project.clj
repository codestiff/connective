(defproject codestiff/connective.malli "0.1.2-SNAPSHOT"
  :description "a connective validator for malli"
  :url "https://github.com/codestiff/connective"

  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :deploy-repositories [["releases"  {:sign-releases false :url "https://clojars.org"}]
                        ["snapshots" {:sign-releases false :url "https://clojars.org"}]]

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [metosin/malli "0.0.1-20200404.091302-14"]
                 [codestiff/connective.core "0.1.0-SNAPSHOT"]]

  :repl-options {:init-ns connective.malli})
