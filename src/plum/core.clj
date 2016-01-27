(ns plum.core
  (:require [clojure.set :as set]))

(defn join
  "For coll a: [a] -> a"
  [& things]
  (let [non-nil (filter identity things)
        ;; this feels horrible
        thing   (first non-nil)]
    (cond (map? thing)
          (apply merge-with join non-nil)

          (or (list? thing) (vector? thing))
          (reduce into non-nil)

          (set? thing)
          (reduce set/union non-nil))))

(defn- map-kv-binding
  [m]
  (first (filter (fn [[k _]] (symbol? k)) m)))

(declare make-collector)

(defn- map-collector
  [from to]
  (let [[keysym valstructure] (map-kv-binding from)
        mapsym (gensym "map")
        extractor (or (:extract from) `identity)
        where (or (:where from) `true)
        bindings (:let from)
        bound-entries (vec (vals (dissoc bindings :as :rest)))
        binding-clauses (reduce (fn [acc [k v]]
                                  (if (symbol? k)
                                    (into acc [k `(get ~mapsym ~v)])
                                    (case k
                                      :as   (into acc [v mapsym])
                                      :rest (into acc [v `(apply dissoc ~mapsym ~bound-entries)])
                                      (throw (Exception. (str "Unsupported binding " k))))))
                                []
                                bindings)]
    `(fn [~mapsym]
       (let ~binding-clauses
         (if ~where
           ~(if keysym
              `(apply join
                      (map (fn [[~keysym rest#]]
                             (~(make-collector valstructure to) rest#))
                           (~extractor ~mapsym)))
              to))))))

(defn- vec-set-collector
  [from to]
  `(fn [coll#]
     (apply join (map (fn [x#] (~(make-collector (first from) to) x#))
                      coll#))))

(defn- make-collector
  [from to]
  (cond (map? from)
        (map-collector from to)

        (or (vector? from) (set? from))
        (vec-set-collector from to)

        :else
        `(fn [~from] ~to)))

(defmacro collector
  [from to]
  (make-collector from to))

(defmacro collect
  [from to coll]
  (let [f (make-collector from to)]
    `(~f ~coll)))

(defn key=
  [& keys]
  #(select-keys % keys))

(defn key!=
  [& keys]
  #(apply dissoc % keys))
