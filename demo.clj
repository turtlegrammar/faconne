;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Include
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is generated from demo.clj.

(ns demo
  (:require [faconne.core :as f]))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Overview/Motivation
;;;;;;;;;;;;;;;;;;;;;;;;

;;; `f/transformer` creates a function from a domain 'schema' -- a map/vector/set
;;; of symbols -- and a range 'schema'.

(def swap-key-order (f/transformer {k1 {k2 v}} {k2 {k1 v}}))

(swap-key-order {:a {:z 1 :y 2} :y {:a 6 :z 9}})
;; => {:z {:a 1, :y 9}, :y {:a 2}, :a {:y 6}}

;;; concat can be written as its type signature: `[[a]] -> [a]`
(def concat' (f/transformer [[a]] [a]))

(concat' [[1 2] '(3 4) [5 6]])
;; => [1 2 3 4 5 6]

;;; Here's a more realistic example.
;;; Suppose we have some JSON data that looks like this:

(def real-world-json
  [{:store-name "Tom's Records"
     :location "1234 Main Street"
     :stock [{:artist "Bartók"
              :title "String Quartets"
              :quantity 5}
             {:artist "Ligeti"
              :title "Violin Concerto"
              :quantity 1}]}
   {:store-name "Roger's Records"
    :location "789 Secondary Street"
    :stock [{:artist "Ligeti"
             :title "Violin Concerto"
             :quantity 3}
            {:artist "Scriabin"
             :title "12 Etudes"
             :quantity 2}]}])

;;; But what we really want is a map from artists to titles to
;;; the stores it's at and in what quantities it's available.

(def real-world-json-transformer
  ;; if the keys were strings instead of keywords, you could use
  ;; {"store-name" store, ...}
  (f/transformer [{:store-name store
                   :location loc
                   :stock [{:keys [artist title quantity]}]}]
                 {artist {title [[(str store " @ " loc) quantity]]}}))

(real-world-json-transformer real-world-json)
;; =>
{"Bartók"
 {"String Quartets"
  [["Tom's Records @ 1234 Main Street" 5]]},

 "Ligeti"
 {"Violin Concerto"
  [["Tom's Records @ 1234 Main Street" 1]
   ["Roger's Records @ 789 Secondary Street" 3]]},

 "Scriabin"
 {"12 Etudes"
  [["Roger's Records @ 789 Secondary Street" 2]]}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Destructuring in the Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; You can destructure elements in the domain in the same way
;;; you would in a `let` or `doseq` binding. This includes:
;;; `{(:keys|:syms|:strs) [...] :as ...}` and `{sym1 literal1 ... symk literalk}` for maps
;;; and `[p]` or `[p1 ... pk]` for vectors/lists treated as tuples (or taken k at a time -- but
;;; that's described later).

(def group-by-sum-of-dice (f/transformer {{:keys [name]} [roll1 roll2]}
                                         ;; the range can contain arbitrary expressions
                                         {(+ roll1 roll2) #{(clojure.string/upper-case name)}}))

(group-by-sum-of-dice {{:id "A75" :name "Gepopo"}         [3 5]
                       {:id "B82" :name "Sugar Plum Fairy"} [2 6]
                       {:id "C92" :name "Tom"}              [1 1]})
;; => {8 #{"GEPOPO" "SUGAR PLUM FAIRY"}, 2 #{"TOM"}}


;;; This takes a vector of records -- like one you'd get from JDBC --
;;; and turns it into a nested map:
(def by-x-and-y (f/transformer [{:keys [x y] :as everything}]
                               {x {y everything}}))

(by-x-and-y [{:x 1 :y 2 :z 3} {:x 1 :y 3 :z 3} {:x 2 :y 2 :z 4}])
;; =>
{1
 {2 {:x 1, :y 2, :z 3},
  3 {:x 1, :y 3, :z 3}},
 2
 {2 {:x 2, :y 2, :z 4}}}

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Destructuring n-tuples is a special case of destructuring a sequence taken n at a time:
;;;;;;;;;;;;;;;;;;;;;;;;;

(def every-first-and-third-in-every-second
  (f/transformer [_ [a _ b]] [a b]))

(every-first-and-third-in-every-second [[1 2 3 4 5 6] [7 8 9 10 11 12]
                                        [13 14 15 15 17 18] [19 20 21 22 23 24]])
;; => [7 9 10 12 19 21 22 24]

(def sums-of-every-others-in-every-second-of-three
  (f/transformer [_ [a b c d] _] [(+ a c) (+ b d)]))

(sums-of-every-others-in-every-second-of-three [[1 2 3 4] [5 6 7 8 9 10 11 12] [13 14 15 16]])
;; => [12 14 20 22] (which is [(+ 5 7) (+ 6 8) (+ 9 11) (+ 10 12)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key Literals (and ambiguities)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are four reserved keywords:  `:keys`, `:syms`, `:strs`, `:as`.
;;; To indicate that any of these are actually map keys,
;;; or to use something besides a string/keyword as a key literal,
;;; use `(:literal key)`:

(f/transform {:keys 1, [1 2] 2, :x [3 4]}
             {(:literal :keys) n, (:literal [1 2]) k, :x [j]}
             [(+ n k j)])
;; => [6, 7] via [(+ 1 2 3) (+ 1 2 4)]

;;; Similarly, if you want to use a variable literal, you need `(:literal ...)`
(defn merge-key-vals [m k1 k2]
  (f/transform m
               {(:literal k1) [v1]
                (:literal k2) [v2]}
               #{v1 v2}))

(merge-key-vals {:a [1 2 3], :b [3 4 5], :c [5 6 7]} :a :b)
;; => #{1 2 3 4 5}

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Filtering (:where)
;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add a `:where` clause -- vector of clojure expressions -- to
;;; collect only items for which all the expressions are true.
;;; the `:where`'s clauses can be aribtary expressions and contain
;;; variables bound in the domain.
(defn filter' [p coll]
  (f/transform coll [a] [a] :where [(p a)]))

(filter' #(< % 5) [3 4 5 6]) ;; => [3 4]


;;; Faconne is intelligent enough to check if predicates are true
;;; as soon as possible. Below, the check for (even? k) happens right
;;; after k is bound to a key, before the corresponding value vector is
;;; traversed and checked for odd elements. If Faconne waited until it
;;; encountered leaves to check predicates, the example wouldn't terminate.
(f/transform {1 (range), 2 [1, 2], 3 (range), 4 [3, 4]}
             {k [n]} {n k}
             :where [(even? k) (odd? n)])
;; => {1 2, 3 4}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Cartesian Product & Beyond of key/val Pairs in a Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For a simple example, suppose we have a map from keywords to integers
;;; and want to find every pair of keywords whose corresponding values sum to 10:

(def keys-whose-vals-sum-to-10
  (f/transformer {k v, k' v'} #{#{k k'}} :where [(not= k k') (= 10 (+ v v'))]))

(keys-whose-vals-sum-to-10 {:a 7, :b 3, :c 5, :d 5, :e 3})
;; => #{#{:c :d} #{:e :a} #{:b :a}}

;;; And you're not limited to taking only all pairs, although
;;; I'm not sure why you'd ever do something like this:
(def three-keys-whose-vals-sum-to-12
  (f/transformer {k v, k' v', k'' v''} #{#{k k' k''}}
                 :where [(not= k k') (not= k' k'') (not= k k'')
                         (= 12 (+ v v' v''))]))

(three-keys-whose-vals-sum-to-12 {:a 4, :b 6, :c 2, :d 4})
;; => #{#{:c :b :a} #{:c :b :d}}
