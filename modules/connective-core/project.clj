(defproject codestiff/connective.core "0.1.4"
  :description "An API for each accessing persisted entities and the respective relationships."
  :url "https://github.com/codestiff/connective"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.taoensso/timbre "4.10.0"]]

  :repl-options {:init-ns connective.core})
