  Faconneeeeiiiigh is a real workhorse!  ![logo](faconne_logo.png)  Logo courtesy of [Clayton Belcher](http://claytonbelcher.com/).  This README is generated from `demo.clj`.
## Include
  Lein: `[faconne "1.0.2"]`  Import:
```clj
(ns demo
  (:require [faconne.core :as f]))

```

## Overview/Motivation
  Often when working with Clojure, you need to transform one nested collection  into another. Faconne makes this as easy as visually specifying the input domain and  output range. Here are some simple examples:
```clj
(def swap-key-order (f/transformer {k1 {k2 v}} ;; domain
                                   {k2 {k1 v}})) ;; range

(def directories {"Pictures" {"2011" ["Lamb.jpg", "Lambda.jpg"]
                              "2012" ["Sheep.jpg", "Sheepda.jpg"]}
                  "Documents" {"2011" ["Effects of Industrialization on Lamb Populations"
                                       "A History of Lamb-Related Cults"]
                               "2013" ["Best of the Interdisciplinary Lamb Conference '98"
                                       "Wool Socks: A Memoir"]}})

(swap-key-order directories)
;; =>
{"2011"
 {"Pictures" ["Lamb.jpg"
              "Lambda.jpg"],
  "Documents" ["Effects of Industrialization on Lamb Populations"
               "A History of Lamb-Related Cults"]},
 "2012" {"Pictures" ["Sheep.jpg"
                     "Sheepda.jpg"]},
 "2013" {"Documents" ["Best of the Interdisciplinary Lamb Conference '98"
                      "Wool Socks: A Memoir"]}}

;; this can also be written using `f/transform`
(f/transform directories {k1 {k2 v}} {k2 {k1 v}})


(def concat-even-indexed (f/transformer [[a _]] ;; inner vector taken two at a time
                                        [a]))

(concat-even-indexed [[1 2 3 4 5] '(6 7 8) [9 10 11 12]])
;; =>
[1 3 5 6 8 9 11]

```
  Here's a more realistic example.  Suppose we have some JSON data that looks like this:
```clj
(def real-world-json
  [{:store-name "Tom's Records"
    :location "1234 Main Street"
    :stock [{:artist "Bartók"
             :title "String Quartets"
             :quantity 2}
            {:artist "Ligeti"
             :title "Violin Concerto"
             :quantity 6}]}
   {:store-name "Sarah's Records"
    :location "789 Secondary Street"
    :stock [{:artist "Ligeti"
             :title "Violin Concerto"
             :quantity 3}
            {:artist "Ligeti"
             :title "Piano Concerto"
             :quantity 7}
            {:artist "Scriabin"
             :title "12 Etudes"
             :quantity 4}]}])

```
  But we're going to be querying the data by artist and title mostly, so  what we really want is a map: artists -> titles -> stores -> quantity.  To make it more contrived, we care only about records available in quantities greater than 2.  This is straightforward with Faconne's map destructuring.
```clj
(def real-world-json-transformer
  ;; if the keys were strings instead of keywords, you could use {"store-name" store, ...}
  (f/transformer [{:store-name store
                   :location loc
                   :stock [{:keys [artist title quantity]}]}] ;; :keys has same meaning as in `let`

                 {artist {title {(str store " @ " loc) quantity}}}

                 :where [(> quantity 2)])) ;; and'd together

(real-world-json-transformer real-world-json)
;; =>
{"Ligeti"
 {"Violin Concerto"
  {"Tom's Records @ 1234 Main Street" 6,
   "Sarah's Records @ 789 Secondary Street" 3},
  "Piano Concerto"
  {"Sarah's Records @ 789 Secondary Street" 7}},
 "Scriabin"
 {"12 Etudes"
  {"Sarah's Records @ 789 Secondary Street" 4}}}


```
  An example motivated by JDBC:
```clj
(def zoo-records [{:id 235632, :type "Lamb", :name "Tom"}
                  {:id 354232, :type "Walrus", :name "Martin"}
                  {:id 241111, :type "Lamb", :name "Sarah"}
                  {:id 999333, :type "Walrus", :name "Emily"}])

(f/transform zoo-records
             [{:keys [type] :as m}] ;; could also be [{:type type :as m}]
             {(clojure.string/lower-case type) [m]})
;; =>
{"lamb" [{:id 235632, :type "Lamb", :name "Tom"}
         {:id 241111, :type "Lamb", :name "Sarah"}],

 "walrus" [{:id 354232, :type "Walrus", :name "Martin"}
           {:id 999333, :type "Walrus", :name "Emily"}]}

```

## Iterators
  If you don't want to transform a data structure, but instead want to do something  at each leaf, you can use `f/iterator`. It's similar to `f/transformer`, but instead  of specifying a range schema, you specify an action (which is any Clojure experession).
```clj
;; uses real-world-json example from first section
(def print-albums
  (f/iterator [{:store-name store
                :location loc
                :stock [{:keys [artist title quantity]}]}]
              (prn (str "You can buy " artist "'s" " album " title " at " store " (" loc ")."))
              :where [(> quantity 0)]))


(print-albums real-world-json)
;; prints "You can buy Bartók's album String Quartets at Tom's Records (1234 Main Street)."  etc.

```
  `f/for-each` is to `f/iterator` what `f/transform` is to `f/transformer`.
```clj
(f/for-each [1 2 3] [a] (prn (inc a)) :where [(even? a)])
;; => prints "3"

```

## Map Semantics
  There are two types of bindings: one to indicate  that you want to traverse every key, and one to indicate you want to traverse a known key.  To traverse every key, the map key can be anything that you'd use as an lvalue  in a `doseq` or `let`.  This includes symbols, map destructuring forms like `{:keys [sym]}` or `{sym :key}`,  and vector destructuring forms like `[s1 ... s2]`. The value can be any valid domain.  This type of key-value `{k v}` pair means "for each key `k` in the map, traverse  its corresponding key `v`."
```clj
(def group-by-sum-of-dice (f/transformer {{:keys [name]} [roll1 roll2]}
                                         {(+ roll1 roll2) #{(clojure.string/upper-case name)}}))

(group-by-sum-of-dice {{:id "A75" :name "Gepopo"}           [3 5]
                       {:id "B82" :name "Sugar Plum Fairy"} [2 6]
                       {:id "C92" :name "Tom"}              [1 1]})
;; =>
{8 #{"GEPOPO" "SUGAR PLUM FAIRY"}, 2 #{"TOM"}}

(def by-rows (f/transformer {[row col] item}
                            {row #{item}}))

(by-rows {[12, 10] :house, [12, 8] :lamb,
          [10, 8]  :goat,  [10, 5] :sheep})
;; =>
{12 #{:lamb :house}, 10 #{:goat :sheep}}


```
  To indicate that you want to bind against or traverse a known key,  use a keyword, a string, or `(:literal <anything>)` map key.
```clj
(def destructuring-example
  {:keys   "lamb"      ;; {(:literal :keys) lamb}  -> lamb = "lamb"
   :strs   "goat"      ;; {(:literal :strs) goat}  -> goat = "goat"
   :syms   "a lamb"    ;; {(:literal :syms) alamb} -> alamb = "a lamb"
   :as     "a goat"    ;; {(:literal :as) agoat}   -> agoat = "a goat"

   :lamb1  "lamb1"     ;; {:keys [lamb1 lamb2]}    -> lamb1 = "lamb1", etc.
   :lamb2  "lamb2"     ;; {:lamb2 somelamb}        -> somelamb = "lamb2"

   'sheep1 "sheep1"    ;; {:syms [sheep1 sheep2]}  -> sheep1 = "sheep1", etc.
   'sheep2 "sheep2"

   "yarn1" "knitting"  ;; {:strs [yarn1 yarn2]}    -> yarn1 = "knitting", etc.
   "yarn2" "weaving"   ;; {"yarn2" yarn2}          -> yarn2 = "weaving"

                       ;; {:as m}                  -> m = {:keys "lamb", ..., "yarn2" "weaving}

   [1 3]   "contrived" ;; {(:literal [1 2]) c}     -> c = "contrived"

   })


```
  If you want to use a variable literal (wording?), you need `(:literal ...)`
```clj
(defn merge-key-vals [m k1 k2] ;; better written using core fns, but I need an example
  (f/transform m
               {(:literal k1) [v1]
                (:literal k2) [v2]}
               #{v1 v2}))

(merge-key-vals {:a [1 2 3], :b [3 4 5], :c [5 6 7]} :a :b)
;; => #{1 2 3 4 5}
(merge-key-vals {"a" [1 2 3], "b" [3 4 5], "c" [5 6 7]} "a" "b")
;; => #{1 2 3 4 5}


```
  If there are k non-literal bindings in the map, then all possible k-groupings  of (key, val) pairs are traversed:
```clj
(def keys-whose-vals-sum-to-10
  ;; read the domain as "for all keys k, k' in a map, and for all respective values v, v'"
  (f/transformer {k v, k' v'}
                 #{#{k k'}}
                 :where [(not= k k')
                         (= 10 (+ v v'))]))

(keys-whose-vals-sum-to-10 {:a 7, :b 3, :c 5, :d 5, :e 3})
;; =>
#{#{:c :d} #{:e :a} #{:b :a}}


```

## Vector and Set Semantics
  Any vector can be written as `[domain_1 ... domain_n]`, which means a vector is being partitioned  into chunks of n. An n-tuple is a special case of such a partitioning.
```clj
(def every-first-and-third-in-every-second
  (f/transformer [_ [a _ b]] [a b]))

(every-first-and-third-in-every-second [[1 2 3 4 5 6] [7 8 9 10 11 12]
                                        [13 14 15 15 17 18] [19 20 21 22 23 24]])
;; =>
[7 9 10 12 19 21 22 24]


(def sums-of-every-others-in-every-second-of-three
  (f/transformer [_ [a b c d] _] [(+ a c) (+ b d)]))

(sums-of-every-others-in-every-second-of-three [[1 2 3 4] [5 6 7 8 9 10 11 12] [13 14 15 16]])
;; =>
[12 14 20 22] ;(which is [(+ 5 7) (+ 6 8) (+ 9 11) (+ 10 12)])

```
  Of course, you can use these types of vectors as values in maps also.  Since sets are unordered, a set is limited to the form of `#{domain}`.
## Filtering (:where)
  Add a `:where` clause -- vector of clojure expressions -- to  collect only items for which all the expressions are true.  the `:where`'s clauses can be aribtary expressions and contain  variables bound in the domain.
```clj
(defn filter' [p coll]
  (f/transform coll [a] [a] :where [(p a)]))

(filter' #(< % 5) [3 4 5 6]) ;; => [3 4]


```
  Faconne is intelligent enough to check if predicates are true  as soon as possible. Below, the check for (even? k) happens right  after k is bound to a key, before the corresponding value vector is  traversed and checked for odd elements. If Faconne waited until it  encountered leaves to check predicates, the example wouldn't terminate.
```clj
(f/transform {1 (range), 2 [1, 2], 3 (range), 4 [3, 4]}
             {k [n]} {n k}
             :where [(even? k) (odd? n)])
;; => {1 2, 3 4}


```

## Efficiency
  Faconne is reasonably efficient. When iterating over sequences, it tends  to be about comparable to handwritten loops. It's faster than threading through  sequence fns like `partition` and `map`. When iterating over maps, Faconne  tends to be faster since it produces less short-lived garbage.  Here are some benchmarks:
```clj
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

(def faconne-swap-key-order (f/transformer {k1 {k2 v}} {k2 {k1 v}}))

(defn handwritten-swap-key-order
  [m]
  (apply merge-with merge
         (map (fn [[k1 inner]]
                (apply merge-with merge
                       (map (fn [[k2 v]]
                              {k2 {k1 v}})
                            inner)))
              m)))

(defn handwritten-swap-key-order2
  [m]
  (apply merge-with merge
         (map (fn [[k1 inner]]
                (reduce (fn [acc [k2 v]]
                          (assoc-in acc [k2 k1] v))
                        {}
                        inner))
              m)))

(def nested-map (let [r (range 0 500)] (zipmap r (cycle [(zipmap r r)]))))

(def bfsko (fn [] (average-time (count (faconne-swap-key-order nested-map)) 100)))
(def bhsko (fn [] (average-time (count (handwritten-swap-key-order nested-map)) 100)))
(def bhsko2 (fn [] (average-time (count (handwritten-swap-key-order2 nested-map)) 100)))


;; I got:
;;demo> (bfsko) -- faconne
;;446.69657557999983
;;demo> (bhsko) -- handwritten
;;542.4995425099997
;;demo> (bhsko2) -- handwritten 2
;;555.25516914


(def long-seq (range 0 25000))
(def handwritten-even-indexed #(->> % (partition 2) (map first)))
(def handwritten-even-indexed2 #(loop [xs %, result [], skip false]
                                  (cond (empty? xs) result
                                        skip (recur (rest xs) result false)
                                        :else (recur (rest xs) (conj result (first xs)) true))))
(def faconne-even-indexed (f/transformer [a _] [a]))
(def bfei (fn [] (average-time (count (faconne-even-indexed long-seq)) 100)))
(def bhei (fn [] (average-time (count (handwritten-even-indexed long-seq)) 100)))
(def bhei2 (fn [] (average-time (count (handwritten-even-indexed2 long-seq)) 100)))


;; I got:
;;demo> (bfei)
;;1.9608322600000003
;;demo> (bfei)
;;1.5673947800000005
;;demo> (bfei)
;;2.083676179999999
;;demo> (bhei2)
;;1.5211781300000007
;;demo> (bhei2)
;;2.2295336199999993
;;demo> (bhei2)
;;1.72410265
;;demo>(bhei)
;;12.371473129999995
;;demo> (bhei)
;;12.248972580000002
;;demo> (bhei)
;;12.219238020000004


```
  Faconne is not always faster; I've found that `(f/transformer [[a]] [a])` is about half as fast  as `#(reduce into [] %)` due to administrative overhead. But the more convoluted the collection,  the more efficient Faconne will be compared to idiomatic sequence-manipulating code since  Faconne will generate less short-lived garbage.
## End
