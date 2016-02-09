(ns plum.core
  (:require [plum.compile :refer [compile]]))

(defmacro transformer
  [domain range & options]
  (let [where (:where (apply hash-map options))]
    (compile domain range where)))

(defmacro transform
  [x domain range & options]
  `(let [f# (transformer ~domain ~range ~@options)]
     (f# ~x)))

(defmacro print-generated-fn
  [domain range & options]
  (let [where (:where (apply hash-map options))]
    (clojure.pprint/pprint (compile domain range where))))
