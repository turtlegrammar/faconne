(ns newdemo
  (:require [faconne.core :as f]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Include
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lein: `[faconne "1.0.3"]`

;;; Import for Clojure:
(ns demo
  (:require [faconne.core :as f]))

;;; Import for Clojurescript:
;;; ```clj (ns demo (:require [faconne.core :as f :include-macros true]))```


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Overview/Motivation
;;;;;;;;;;;;;;;;;;;;;;;;

;;; Frequently in Clojure, we find ourselves with a deeply nested
;;; collection that we wish had a different shape. So, we write an
;;; ugly nest of map, filter, reduce, group-by, etc. that takes our
;;; collection and re-arranges it, maps deep into it, inverts part of it,
;;; introduces or changes groupings in it, filters deep into it, or tags
;;; child collection with metadata. This is hard to read,
;;; and difficult to update when the shape of the data changes.

;;; Faconne lets you declaratively transform nested collections into
;;; other nested collections by visually specifying the shape
;;; of the input and the desired shape of the output.
;;; For example:

(def student-data
  ;; As we might pull from a database with JDBC
  [{:student "john", :grade 97, :course "math", :campus "east"}
   {:student "john", :grade 90, :course "english", :campus "east"}
   {:student "john", :grade 70, :course "history", :campus "east"}
   {:student "dave", :grade 80, :course "math", :campus "east"}
   {:student "dave", :grade 100 ,:course "english", :campus "east"}
   {:student "mary", :grade 90, :course "math", :campus "west"}
   {:student "mary", :grade 92, :course "english", :campus "west"}
   {:student "mary", :grade 94, :course "history", :campus "west"}])

(defn average [xs] (/ (reduce + 0 xs) (count xs)))

(def organized-student-data
  (f/transform student-data
               [{:keys [student grade course campus]}]
               {campus {:number-students (count #{student})
                        :avg-grade-per-course {course (average [grade])}
                        :student-grades {student {course grade}}}}))

;; (Note that `count`, and `average` are normal Clojure functions
;;  called on values built up in the transform.)
;; =>

{"east" {:number-students 2,
         :avg-grade-per-course {"math" 177/2,
                                "english" 95,
                                "history" 70},
         :student-grades {"john" {"math" 97,
                                  "english" 90,
                                  "history" 70},
                          "dave" {"math" 80,
                                  "english" 100}}},
 "west" {:number-students 1,
         :avg-grade-per-course {"math" 90,
                                "english" 92,
                                "history" 94},
         :student-grades {"mary" {"math" 90,
                                  "english" 92,
                                  "history" 94}}}}

;; We can transform back to the original collection with:
(f/transform organized-student-data
             {campus {:student-grades {student {course grade}}}}
             [{:campus campus, :student student, :course course, :grade grade}])


;;; Faconne is great for tranforming data pulled from a database or API
;;; into something that's easier for your application to work with.


;;;;;;;;;;;;;;;;;;
;;;; Examples ;;;;
;;;;;;;;;;;;;;;;;;

;;; I've used Faconne in a production for several years.
;;; Here's some usages I've encountered using Faconne 'in the wild.'


;;; Un-nesting / un-grouping
(def store-data
  {"Gas Station" {1 ["Hot Dog", "Nachos", "Cola"]
                  2 ["Hot Dog", "Toothpaste", "Deoderant"]}
   "Grocer" {1 ["Celery" "Onion" "Carrot"]
             2 ["Orange" "Apple"]
             3 ["Salmon"]}})
(f/transform store-data
             {store {aisle [product]}}
             {store #{product}})
;; =>
{"Gas Station" #{"Nachos" "Toothpaste" "Cola" "Deoderant" "Hot Dog"}
 "Grocer" #{"Carrot" "Onion" "Celery" "Salmon" "Orange" "Apple"}}


(f/transform store-data
             {store {aisle [product]}}
             {store [{:aisle aisle, :product product}]})
;; =>
{"Gas Station" [{:aisle 1, :product "Hot Dog"}
                {:aisle 1, :product "Nachos"}
                {:aisle 1, :product "Cola"}
                {:aisle 2, :product "Hot Dog"}
                {:aisle 2, :product "Toothpaste"}
                {:aisle 2, :product "Deoderant"}],
 "Grocer" [{:aisle 1, :product "Celery"}
           {:aisle 1, :product "Onion"}
           {:aisle 1, :product "Carrot"}
           {:aisle 2, :product "Orange"}
           {:aisle 2, :product "Apple"}
           {:aisle 3, :product "Salmon"}]}



;;; Nesting / Grouping

(def event-data
  [{:day "2018-08-10", :type "add-user" :handled? false :data ["steve"]}
   {:day "2018-08-10", :type "add-user" :handled? true :data ["george"]}
   {:day "2018-08-10", :type "remove-user" :handled? false :data ["janice"]}
   {:day "2018-08-11", :type "add-user" :handled? true :data ["jocelyn"]}
   {:day "2018-08-11", :type "remove-user" :handled? false :data ["steve"]}])

(f/transform
 event-data
 [{:keys [day type handled? data]}]
 {day {(if handled? :handled :unhandled) {type data}}})
;; =>
{"2018-08-10"
 {:unhandled
  {"add-user" ["steve"],
   "remove-user" ["janice"]},
  :handled {"add-user" ["george"]}}
 , "2018-08-11"
 {:handled {"add-user" ["jocelyn"]},
  :unhandled {"remove-user" ["steve"]}}}

;;; Inverting
(f/transform {"GYU-6749" 1
              "JEI-1353" 2
              "JNMK-194" 3}
             {license-plate parking-space}
             {parking-space license-plate})
;; =>
{1 "GYU-6749", 2 "JEI-1353", 3 "JNMK-194"}


(f/transform {"Grocer" #{"Hot Dog", "Celery", "Tooth Brush"}
              "Gas Station" #{"Hot Dog", "Tooth Brush", "Beer"}}
             {store #{product}}
             {(clojure.string/lower-case product)
              #{(clojure.string/lower-case store)}})
;; =>
{"celery" #{"grocer"},
 "tooth brush" #{"gas station" "grocer"},
 "hot dog" #{"gas station" "grocer"},
 "beer" #{"gas station"}}



;;; Mapping and Merging
(f/transform {"First Baseman" [{:first-name "Steve" :last-name "White"}]
              "first baseman" [{:first-name "Mark" :last-name "Smith"}]
              "second Baseman" [{:first-name "George" :last-name "Brown"}]}
             {position [{:first-name f :last-name l}]}
             {(-> position
                  clojure.string/lower-case
                  (clojure.string/replace " " "-")
                  keyword)
              [(str l ", " f)]})
;; =>
{:first-baseman ["White, Steve" "Smith, Mark"],
 :second-baseman ["Brown, George"]}

;;; Filtering


(def franchise-info
  [{:franchise "Laundry Store"
    :location {:name "West Location"}
    :managers [{:name "Ruth", :months-worked 15}
               {:name "Bruno", :months-worked 1}]
    :employees [{:name "Luke", :months-worked 0}]}
   {:franchise "Laundry Store"
    :location {:name "East Location"}
    :managers [{:name "Tomas", :months-worked 8}
               {:name "Ruth", :months-worked 15}]
    :employees [{:name "Mary", :months-worked 22}]}
   {:franchise "Restaurant"
    :location {:name "Campus Location"}
    :managers [{:name "Sarah", :months-worked 6}
               {:name "Emily", :months-worked 22}]
    :employees [{:name "Joe", :months-worked 0}
                {:name "Nathan", :months-worked 1}]}])

;; Every Laundry Store Location that has one manager who's worked at least a year:
(f/transform franchise-info
             [{:franchise franchise
               :location {:name location}
               :managers [{:months-worked months}]}]
             #{location}
             ;; the :where keyword accepts a vector of clojure expressions
             ;; that may contain symbols bound when describing the input
             ;; shape. The vector is implicitly and'd together.
             :where [(> months 12)
                     (= franchise "Laundry Store")])
;; =>
#{"West Location"}

;; Every employee who's worked longer than a year and has Ruth as
;; a manager, mapped to the location they work at.
(f/transform franchise-info
             [{:franchise franchise
               :location {:name location}
               :managers [{:name manager}]
               :employees [{:name employee, :months-worked months}]}]
             {employee {:months-worked months
                        :location location
                        :franchise franchise}}
             :where [(= manager "Ruth")
                     (> months 12)])
;; =>
{"Mary"
 {:months-worked 22,
  :location "East Location",
  :franchise "Laundry Store"}}

;; Fun fact:
;; Faconne evaluates each where clause independently as soon as possible
;; to avoid needlessly traversing branches of your data structure
(f/transform {1 (range) ;; infinite sequence
              2 [2 5]
              3 (range) ;; infinite sequence
              4 [7 10]}
             {k [v]}
             [v]
             :where [(even? k) (odd? v)])
;; =>
[5 7]

;;; Reducing Collections Built up in Transform



;; todo


;;;;;;;;;;;;;;;;;;;;;;
;;;; More Details ;;;;
;;;;;;;;;;;;;;;;;;;;;;

;;; Vectors:




;;;;;;;;;;;;;;;;;;;;;
;;;; Closing Pun ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Faconneeeeiiiigh is a real workhorse!
;;; ![logo](faconne_logo.png)
;;; Logo courtesy of [Clayton Belcher](http://claytonbelcher.com/).
