(ns faconne.core
  (:require [faconne.compile :refer [genfn]]
            [clojure.pprint :refer [pprint]]))

(defn- options-map
  [options]
  (let [m (apply hash-map options)
        unsupported (-> m (dissoc :where))]
    (if-not (empty? unsupported)
      (throw (Exception. (str "Unsupported options to transform(er): "
                              unsupported "; only `:where` is supported.")))
      m)))

(defmacro transformer
  [domain range & options]
  ;; throw error on unsupported keywords
  (let [where (:where (options-map options))]
    (genfn domain range where)))

(defmacro transform
  [x domain range & options]
  `(let [f# (transformer ~domain ~range ~@options)]
     (f# ~x)))

(defmacro print-generated-fn
  [domain range & options]
  (let [where (:where (options-map options))]
    (pprint (genfn domain range where))))
