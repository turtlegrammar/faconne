(ns faconne.parse-range
  (:require [clojure.set :as set]
            [clojure.pprint :as pprint]
            [faconne.util :refer [error]]
            [clojure.walk :as walk]
            #?(:clj [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])))


(defn spy [x] (pprint/pprint x) x)

;; Reduce Data represents a collection that needs to be reduced after being
;; fully created.
;; For example, (f/transform [1 2 3] [x] (apply max [x]))
;; requires reducing logic on [x] in the range, since we're running
;; `apply max` on it after creating it.
(defn make-reduce-data
  [reducefn data type]
  ;; A function that has a number of arguments equal to the number of
  ;; substructures in data. Used after data is fully built.
  {::reducefn reducefn
   ;; A collection of substructures (collection built up over the transform)
   ;; that the reducefn will act on
   ::data data
   ;; leaf (no more reducedatas down this branch), reduce (custom
   ;; reducing fn), map, vector, set
   ::type type})

;; This is hacky -- is there a better way to do this?
;; I tried a record, but (= (type x) RecordData) didn't always work.
(defn reducedata? [x] (boolean (::type x)))

(defn deep-merge
  [x y]
  (cond (reducedata? x)
        ;; Both have the same reducefn
        (make-reduce-data (::reducefn x)
                          (if (= (::type x) :leaf)
                            ;; Leaves are collections with no more
                            ;; reducedatas in them. They're built by
                            ;; the transform, so we deep merge, as if
                            ;; doing a normal transform.
                            (deep-merge (::data x) (::data y))
                            ;; Otherwise data is a collection of substructures,
                            ;; meaning each element in data is a separate coll
                            ;; being built by the transform.
                            ;; So we piece-wise deep-merge these.
                            (mapv deep-merge (::data x) (::data y)))
                          (::type x))

        (and (map? x) (map? y))
        (merge-with deep-merge x y)

        (and (coll? x) (coll? y))
        (into x y)

        :else y))

(defn eval-reduce-data
  "Now that the reducedata's substructures are fully built, apply
  the reducing functions to them."
  [reduce-data]
  ;; This could possibly be made more efficient by manually walking
  ;; the data structure, since we don't need to further traverse leaves,
  ;; and leaves are likely to be collection with many elements.
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
  "Find the topmost child forms that use collections built in the transform.

  For example, if we have (max (apply max [g1]) (apply max [g2])) and both
  g1 and g2 are domain-bound, then this returns
  [[g1] [g2]].

  By replacing these substructures with gensyms, we get the reducing fn:
  (fn [g1-sym g2-sym] (max (apply max g1-sym) (apply max g2-sym)))

  (see build-reducer)."
  [domain-bound-symbols form]
  (if (uses-symbol? domain-bound-symbols form)
    (if (list? form)
      (->> (mapv #(reducable-substructures domain-bound-symbols %) form)
           (reduce into []))
      [form])
    []))

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
               ;; If the child is a list, that means there's a function call.
               (and (list? child)
                    ;; A function call doesn't necessitate reducing logic by
                    ;; itself. For example (transform [x] [(inc x)]) doesn't
                    ;; reduce a collection.
                    ;; So, additionally, there must be a child of the child
                    ;; (called grandchild/gc) that is transformation-built
                    ;; collection (i.e., map/vector/set and uses a domain
                    ;; bound symbol)
                    (some (fn [gc]
                            ;; have to opt-in on the collection
                            (and (:expand (meta gc))
                                 (cond (map? gc)
                                       ;; we don't support transform logic in keys
                                       (some #(uses-symbol? domain-bound-symbols %) (vals gc))

                                       (or (set? gc) (vector? gc))
                                       (uses-symbol? domain-bound-symbols gc)

                                       :else false)))
                          grandchildren))))
           children))))

(defn convert-range-to-reducable
  [domain-bound-symbols form]
  ;; The strategy for dealing with ranges that have reducing logic is
  ;; to build up a result of reducedata objects instead of native clojure
  ;; collections, then evaluate them (i.e., run the reducers) at the end
  ;; of the transform, once all the collections have been fully built.
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

                      ;; We have a function call, and we know from above we require reducing logic.
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
        ;; this is always going to be a map, since reducedata is a map,
        ;; and we're building a reducedata object which we'll evaluate at the end.
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
