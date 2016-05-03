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

(defn handwritten-swap-key-order
  [m]
  (apply merge-with merge
         (map (fn [[k1 inner]]
                (apply merge-with merge
                       (map (fn [[k2 v]]
                              {k2 {k1 v}}) inner))) m)))

;; in milliseconds
(defmacro time-it-takes
  [exp]
  `(let [start# (. java.lang.System (clojure.core/nanoTime))]
     ~exp
     (let [end# (. java.lang.System (clojure.core/nanoTime))]
       (/ (- end# start#) 1000000.0))))

(defmacro average-time
  [exp trials]
  `(loop [t# ~trials sum# 0.0]
     (if (<= t# 0) (/ sum# ~trials)
         (recur (dec t#) (+ sum# (time-it-takes ~exp))))))
