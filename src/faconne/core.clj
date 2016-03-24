(ns faconne.core
  (:require [faconne.compile :refer [genfn]]
            [clojure.pprint :refer [pprint]]))

(defmacro transformer
  [domain range & options]
  ;; throw error on unsupported keywords
  (let [where (:where (apply hash-map options))]
    (genfn domain range where)))

(defmacro transform
  [x domain range & options]
  `(let [f# (transformer ~domain ~range ~@options)]
     (f# ~x)))

(defmacro print-generated-fn
  [domain range & options]
  (let [where (:where (apply hash-map options))]
    (pprint (compile domain range where))))