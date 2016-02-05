(ns demo
  (:require [plum.core :as p]))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overview
;;;;;;;;;;;;;;;;;;;;;;;;

;; `p/transformer` creates a function from a domain 'schema' -- a map/vector/set
;; of symbols -- and a range 'schema'.

(def swap-key-order (p/transformer {k1 {k2 v}} {k2 {k1 v}}))

(swap-key-order {:a {:z 1 :y 2} :y {:a 6 :z 9}})
;; => {:z {:a 1, :y 9}, :y {:a 2}, :a {:y 6}}

;; flatten can be written from as its type signature: [[a]] -> [a]
(def flatten (p/transformer [[a]] [a]))

(flatten [[1 2] '(3 4) [5 6]])
;; => [1 2 3 4 5 6]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructuring in the Domain
;;;;;;;;;;;;;;;;;;;;;;;;

;; Generally, you can destructure elements in the domain in the same way
;; you would in a `let` or `doseq` clause.

;; Here we transform a map (from person records - maps of id and name -
;; to last known locations - pair of latitude, longitude) to a map from
;; longitudes to people who have been there recently.
(def invade-privacy (p/transformer {{:keys [name]} [_ lon]}
                                   {lon #{name}}))

(invade-privacy {{:id "A75" :name "Slothrop"}         [89.67 45.21]
                 {:id "B82" :name "Sugar Plum Fairy"} [56.78 45.21]
                 {:id "C92" :name "Tom"}              [56.78 86.3]})
;; => {45.21 #{"Slothrop" "Sugar Plum Fairy"}, 86.3 #{"Tom"}}

;; Alternatively, you can use the {name :name, id :id} style instead of {:keys [id name]}

;;;;;;
;; Destructuring n-tuples is a special case of destructuring a sequence taken n at a time:
;;;;;;

;;;;;;
;; Key Literals (and ambiguities)
;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering
;;;;;;;;;;;;;;;;;;;;;;;;

;;(defn filter' [p coll]
;;  (p/transform coll [a] [a]
;;               :where [(p a)]))
;;

;; predicates evalled as soon as possible


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arbitrary Expressions in the range
;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn map' [f coll] (p/transform coll [a] [(f a)]))
