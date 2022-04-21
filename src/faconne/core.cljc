(ns faconne.core
  (:require [faconne.compile :refer [gen-iterator gen-transformer]]
            [faconne.util :refer [error]]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  [input-data-file output-data-file transform-file]
  (let [data (-> input-data-file slurp json/read-str)
        [domain range & [where]] (-> transform-file slurp str/split-lines)
        f-code (edn/read-string (str "(faconne.core/transformer " domain " " range " " (if where (str ":where " where) "") ")"))
        f (eval f-code)
        result (f data)
        output-json (json/write-str result)]
    (spit output-data-file output-json)))

(defn- options-map
  [options]
  (let [m (apply hash-map options)
        unsupported (-> m (dissoc :where))]
    (if-not (empty? unsupported)
      (error (str "Unsupported options to transform(er): "
                  unsupported "; only `:where` is supported."))
      m)))

(defmacro transformer
  [domain range & options]
  (let [where (:where (options-map options))]
    (gen-transformer domain range where)))

(defmacro iterator
  [domain action & options]
  (let [where (:where (options-map options))]
    (gen-iterator domain action where)))

(defmacro transform
  [x domain range & options]
  `(let [f# (transformer ~domain ~range ~@options)]
     (f# ~x)))

(defmacro for-each
  [x domain action & options]
  `(let [f# (iterator ~domain ~action ~@options)]
     (f# ~x)))

(defmacro print-generated-transformer
  [domain range & options]
  (let [where (:where (options-map options))]
    (pprint (gen-transformer domain range where))))

(defmacro print-generated-iterator
  [domain action & options]
  (let [where (:where (options-map options))]
    (pprint (gen-iterator domain action where))))
