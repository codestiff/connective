(defproject codestiff/connective.malli "0.1.10-SNAPSHOT"
  :description "a connective validator for malli"
  :url "https://github.com/codestiff/connective"

  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [metosin/malli "0.2.1"]
                 [codestiff/connective.core "0.1.8"]]

  :repl-options {:init-ns connective.malli})
