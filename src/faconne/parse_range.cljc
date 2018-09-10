(ns faconne.parse-range
  (:require [clojure.set :as set]
            [clojure.pprint :as pprint]
            [faconne.util :refer [error]]
            [clojure.walk :as walk]
            #?(:clj [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])))

;; This namespace handles

(defn spy [x] (pprint/pprint x) x)

;; Reduce Data represents
(defn make-reduce-data
  [reducefn data type]
  {::reducefn reducefn
   ::data data
   ::type type})

(defn reducedata? [x] (boolean (::type x)))

(defn deep-merge
  [x y]
  (cond (reducedata? x)
        (make-reduce-data (::reducefn x)
                          (if (= (::type x) :leaf)
                            (deep-merge (::data x) (::data y))
                            (mapv deep-merge (::data x) (::data y)))
                          (::type x))

        (and (map? x) (map? y))
        (merge-with deep-merge x y)

        (and (coll? x) (coll? y))
        (into x y)

        :else y))

(defn eval-reduce-data
  [reduce-data]
  (walk/postwalk (fn [x]
                   (if (reducedata? x)
                     (case (::type x)
                       :vector (::data x)
                       :map (reduce deep-merge (::data x))
                       :set (set (::data x))
                       :leaf (::data x)
                       :reduce (apply (::reducefn x) (::data x)))
                     x))
                 reduce-data))

(defn child-forms
  [form]
  (let [result (atom [])]
    (walk/postwalk #(do (swap! result conj %) %) form)
    @result))

(defn uses-symbol?
  [domain-bound-symbols form]
  (not-empty
   (set/intersection domain-bound-symbols (set (child-forms form)))))

(defn reducable-substructures
  [domain-bound-symbols form]
  (let [substructures (atom [])]
    (letfn [(go [child-form]
                (when (uses-symbol? domain-bound-symbols child-form)
                  (cond (list? child-form) (mapv go child-form)
                        :else (swap! substructures conj child-form))))]
      (go form))
    @substructures))

(defn build-reducer
  [substructures form]
  (let [params (mapv (fn [_] (gensym)) substructures)]
    `(fn ~params ~(walk/postwalk-replace (zipmap substructures params) form))))

(defn requires-reducing-logic?
  [domain-bound-symbols form]
  (let [children (child-forms form)]
    (boolean
     (some (fn [child]
             (let [grandchildren (child-forms child)]
               (and (list? child)
                    (some (fn [gc]
                            (cond (map? gc)
                                  (some #(uses-symbol? domain-bound-symbols %) (vals gc))

                                  (or (set? gc) (vector? gc))
                                  (uses-symbol? domain-bound-symbols gc)

                                  :else false))
                          grandchildren))))
           children))))

(defn convert-range-to-reducable
  [domain-bound-symbols form]
  (letfn [(go [range]
              (if (requires-reducing-logic? domain-bound-symbols range)
                (cond (map? range)
                      `(make-reduce-data
                        nil
                        ~(->> range
                              (map (fn [[k v]] {k (go v)}))
                              (apply merge)
                              vector)
                        :map)

                      (vector? range)
                      `(make-reduce-data
                        nil
                        ~(mapv go range)
                        :vector)

                      (set? range)
                      `(make-reduce-data
                        nil
                        ~(mapv go range)
                        :set)

                      (list? range)
                      (let [substructures (reducable-substructures domain-bound-symbols range)
                            reducer (build-reducer substructures range)]
                        `(make-reduce-data ~reducer ~(mapv go substructures) :reduce))

                      :else `(make-reduce-data nil [~range] :leaf))

                `(make-reduce-data nil ~range :leaf)))]
    (go form)))



;; `xs` is always known at compile time, so this can be a macro to avoid some
;; recursive function overhead.
(defmacro into!
  [coll xs]
  (loop [xs xs, result coll]
    (if (empty? xs)
      result
      (recur (rest xs) `(conj! ~result ~(first xs))))))


(defn build-clauses-from-range
  "The strategy: In general, maintain a volatile reference to the result.

  If the range is a vector/set, the result will be a transient []/#{}.
  At each leaf, evaluate the range and 'vswap! into!' it into the maintained result.
  At the end of the fn, just call persistent! on the result.
  You might think the volatile ref is extraneous since we're using transients, but
  fns like `conj!` are meant to be used for the result they return, not for their side-effects.
  (This caused a weird bug.)

  If the range is a map, then the result is a persistent
  map. Initially, I had created functions like assoc-in! and
  update-in! that worked on nested map transients, but found that
  these were actually slower than just merging a bunch of small
  persistent maps together. I'm not really sure why this was the
  case. Anyway, at each leaf, evaluate the range and deep merge it
  into the result. At the end, just return the result. "
  [domain-bound-symbols range result-sym]
  (let [use-reducables? (requires-reducing-logic? domain-bound-symbols range)]
    (if use-reducables?
      (let [converted-range (convert-range-to-reducable domain-bound-symbols range)]
        ;; this is always going to be a map
        {:init `(volatile! {})
         :return `(eval-reduce-data (deref ~result-sym))
         :modifier `(vswap! ~result-sym deep-merge ~converted-range)})
      (cond (or (vector? range) (set? range))
            {:init `(volatile! ~(if (vector? range) `(transient []) `(transient #{})))
             :return `(vswap! ~result-sym persistent!)
             :modifier (if (= (count range) 1)
                         `(vswap! ~result-sym conj! ~(first range))
                         `(vswap! ~result-sym (fn [x#] (into! x# ~range))))}

            (map? range)
            {:init `(volatile! {})
             :return `(deref ~result-sym)
             :modifier `(vswap! ~result-sym deep-merge ~range)}))))
