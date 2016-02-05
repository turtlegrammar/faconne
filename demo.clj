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

;; You can destructure elements in the domain in the same way
;; you would in a `let` or `doseq` binding. This includes:
;; {(:keys|:syms|:strs) [...] :as ...} and {sym1 literal1 ... symk literalk} for maps
;; and [p] or [p1 ... pk] for vectors/lists treated as pairs (or taken k at a time -- but
;; that's described later).

(def group-by-sum-of-dice (p/transformer {{:keys [name]} [roll1 roll2]}
                                         ;; the range can contain arbitrary expressions
                                         {(+ roll1 roll2) #{(clojure.string/upper-case name)}}))

(group-by-sum-of-dice {{:id "A75" :name "Slothrop"}         [3 5]
                       {:id "B82" :name "Sugar Plum Fairy"} [2 6]
                       {:id "C92" :name "Tom"}              [1 1]})
;; => {8 #{"SLOTHROP" "SUGAR PLUM FAIRY"}, 2 #{"TOM"}}


;; This takes a vector of records -- like one you'd get from JDBC --
;; and turns it into a nested map:
(def by-composer-and-year (p/transformer [{:keys [composer year] :as piece}]
                                         {composer {year [piece]}}))

(by-composer-and-year [{:composer "Bartók" :title "Piano Concerto 1" :year 1926}
                       {:composer "Bartók" :title "String Quartet 2" :year 1917}
                       {:composer "Ligeti" :title "Etude 1" :year 1985}
                       {:composer "Ligeti" :title "Mysteries of the Macabre" :year 1992}])
;; =>
;;{"Bartók"
;; {1926 [{:composer "Bartók", :title "Piano Concerto 1", :year 1926}],
;;  1917 [{:composer "Bartók", :title "String Quartet 2", :year 1917}]},
;; "Ligeti"
;; {1985 [{:composer "Ligeti", :title "Etude 1", :year 1985}],
;;  1992 [{:composer "Ligeti", :title "Mysteries of the Macabre", :year 1992}]}}

;;;;;;
;; Destructuring n-tuples is a special case of destructuring a sequence taken n at a time:
;;;;;;

;;;;;;
;; Key Literals (and ambiguities)
;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering
;;;;;;;;;;;;;;;;;;;;;;;;

;; Add a :where clause -- vector of clojure expressions -- to
;; collect only items for which all the expressions are true.
;; the :where clauses can be aribtary expressions and contain
;; variables bound in the domain.
(defn filter' [p coll]
  (p/transform coll [a] [a] :where [(p a)]))

(filter' #(< % 5) [3 4 5 6]) ;; => [3 4]


;; plum is intelligent enough to check if predicates are true
;; as soon as all symbols used in them are defined. For example, this
;; terminates instantly:
(p/transform {:infinite (range)}
             {k [n]} {n k}
             :where [(= 1 0)])
;; => {}


;; This finds all pairs of students and professors (and the class that links them)
;; where the student and professor have the same name.
;;(def same-names (p/transform {{:name prof-name}
;;                              {:students {:name student-name}
;;                               :name class-name}}
;;                             [{:prof prof-name
;;                               :student student-name
;;                               :class class-name}]
;;                             :where [(= prof-name student-name)
;;                                     (not= prof-name "Meyers")]))
;;(same-names {{:name "Meyers"
;;              :id "A820"}
;;             {:name "AI"
;;              :students [""]}})
;;