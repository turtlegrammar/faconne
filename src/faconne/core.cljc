(ns faconne.core
  (:require [faconne.compile :refer [gen-iterator gen-transformer]]
            [faconne.util :refer [error]]
            [clojure.pprint :refer [pprint]]))

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
