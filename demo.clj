

(def concat-even-indexed (f/transformer [[a _]] ;; inner vector taken two at a time
                                        [a]))

(concat-even-indexed [[1 2 3 4 5] '(6 7 8) [9 10 11 12]])
;; =>
[1 3 5 6 8 9 11]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Iterators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If you don't want to transform a data structure, but instead want to do something
;;; at each leaf, you can use `f/iterator`. It's similar to `f/transformer`, but instead
;;; of specifying a range schema, you specify an action (which is any Clojure experession).

;; uses real-world-json example from first section
(def print-albums
  (f/iterator [{:store-name store
                :location loc
                :stock [{:keys [artist title quantity]}]}]
              (prn (str "You can buy " artist "'s" " album " title " at " store " (" loc ")."))
              :where [(> quantity 0)]))


(print-albums real-world-json)
;; prints "You can buy Bartók's album String Quartets at Tom's Records (1234 Main Street)."  etc.

;;; `f/for-each` is to `f/iterator` what `f/transform` is to `f/transformer`.

(f/for-each [1 2 3] [a] (prn (inc a)) :where [(even? a)])
;; => prints "3"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Map Semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are two types of bindings: one to indicate
;;; that you want to traverse every key, and one to indicate you want to traverse a known key.

;;; To traverse every key, the map key can be anything that you'd use as an lvalue
;;; in a `doseq` or `let`.
;;; This includes symbols, map destructuring forms like `{:keys [sym]}` or `{sym :key}`,
;;; and vector destructuring forms like `[s1 ... s2]`. The value can be any valid domain.
;;; This type of key-value `{k v}` pair means "for each key `k` in the map, traverse
;;; its corresponding key `v`."

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


;;; To indicate that you want to bind against or traverse a known key,
;;; use a keyword, a string, or `(:literal <anything>)` map key.

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


;;; If you want to use a variable literal (wording?), you need `(:literal ...)`
(defn merge-key-vals [m k1 k2] ;; better written using core fns, but I need an example
  (f/transform m
               {(:literal k1) [v1]
                (:literal k2) [v2]}
               #{v1 v2}))

(merge-key-vals {:a [1 2 3], :b [3 4 5], :c [5 6 7]} :a :b)
;; => #{1 2 3 4 5}
(merge-key-vals {"a" [1 2 3], "b" [3 4 5], "c" [5 6 7]} "a" "b")
;; => #{1 2 3 4 5}


;;; If there are k non-literal bindings in the map, then all possible k-groupings
;;; of (key, val) pairs are traversed:

(def keys-whose-vals-sum-to-10
  ;; read the domain as "for all keys k, k' in a map, and for all respective values v, v'"
  (f/transformer {k v, k' v'}
                 #{#{k k'}}
                 :where [(not= k k')
                         (= 10 (+ v v'))]))

(keys-whose-vals-sum-to-10 {:a 7, :b 3, :c 5, :d 5, :e 3})
;; =>
#{#{:c :d} #{:e :a} #{:b :a}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Vector and Set Semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Any vector can be written as `[domain_1 ... domain_n]`, which means a vector is being partitioned
;;; into chunks of n. An n-tuple is a special case of such a partitioning.

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

;;; Of course, you can use these types of vectors as values in maps also.

;;; Since sets are unordered, a set is limited to the form of `#{domain}`.
