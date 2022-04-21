(defproject faconne "1.1.1"
  :description "Data restructuring DSL"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/data.json "2.4.0"]]
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :main ^:skip-aot faconne.core
  :profiles {:uberjar {:aot :all :uberjar-name "faconne.jar"}})
