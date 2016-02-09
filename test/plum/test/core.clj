(ns plum.test.core
  (:require [plum.core :as p])
  (:use [clojure.test]))

(def profs->classes->students
  {"Sussman" {"AI"
              [{:name "John"
                :grade "A"}
               {:name "Sally"
                :grade "B"}]
              "Compilers"
              [{:name "Tom"
                :grade "B"}
               {:name "John"
                :grade "B"}]}
   "Abelson" {"Machine Learning"
              [{:name "Sally"
                :grade "C"}
               {:name "Tom"
                :grade "B-"}]
              "Compilers"
              [{:name "Eva Lu Ator"
                :grade "B"}
               {:name "Ben Bitdiddle"
                :grade "A"}]}})

(def students->profs {"John" #{"Sussman"} "Sally" #{"Abelson" "Sussman"}
                      "Tom" #{"Abelson" "Sussman"} "Eva Lu Ator" #{"Abelson"}
                      "Ben Bitdiddle" #{"Abelson"}})

(deftest map->map
  (let [m {:a {:b 2 :c 5} :c {:b 3 :e 1}}]
    (is (= (p/transform m
                        {k1 {k2 v}}
                        {k2 {k1 v}})
           {:b {:a 2
                :c 3}
            :c {:a 5}
            :e {:c 1}}))))

(deftest set-in-map
  (is (= (p/transform profs->classes->students
                      {prof {_ [student]}}
                      {(:name student) #{prof}})
         students->profs))

  (is (= (p/transform profs->classes->students
                      {prof {_ [{:keys [name]}]}}
                      {name #{prof}})
         students->profs))

  (is (= (p/transform profs->classes->students
                      {prof {_ [{:name name}]}}
                      {name #{prof}})
         students->profs)))


;;    ;; Map from students to set of classes they have an A in
;;    (is (= (p/collect {prof {class [{:let {student :name
;;                                           grade :grade}}]}}
;;                      {student (if (= grade "A")
;;                                 #{class}
;;                                 #{})}
;;                      m)
;;           {"John" #{"AI"}
;;            "Ben Bitdiddle" #{"Compilers"}
;;            "Eva Lu Ator" #{}
;;            "Sally" #{}
;;            "Tom" #{}}))
;;
;;    (is (= (p/collect {prof {:extract (p/key= "Compilers")
;;                             class [{:let {student :name
;;                                           grade :grade}
;;                                     :where (= grade "A")}]}}
;;                      ;; all students who have an A in Compilers
;;                      #{student}
;;                      m)
;;           #{"Ben Bitdiddle"}))
;;
;;    ;; swap the order of professor and class
;;    (let [class->prof {"AI"
;;                       {"Sussman"
;;                        [{:name "John"
;;                          :grade "A"}
;;                         {:name "Sally"
;;                          :grade "B"}]}
;;                       "Compilers"
;;                       {"Sussman"
;;                        [{:name "Tom"
;;                          :grade "B"}
;;                         {:name "John"
;;                          :grade "B"}]
;;                        "Abelson"
;;                        [{:name "Eva Lu Ator"
;;                          :grade "B"}
;;                         {:name "Ben Bitdiddle"
;;                          :grade "A"}]}
;;                       "Machine Learning"
;;                       {"Abelson"
;;                        [{:name "Sally"
;;                          :grade "C"}
;;                         {:name "Tom"
;;                          :grade "B-"}]}}
;;          swap-key-order (p/collector {k1 {k2 v}} {k2 {k1 v}})]
;;      (is (= (swap-key-order m) class->prof))
;;      (is (= (swap-key-order (swap-key-order m)) m)))))
;;
;;(deftest cumbersome-collections
;;  (let [m [{:class "Compilers"
;;            :professor "Sussman"
;;            :room "West 707"
;;            :students ["Eva Lu Ator" "Ben Bitdiddle"]}
;;           {:class "AI"
;;            :professor "Abelson"
;;            :room "East 103"
;;            :students ["Eva Lu Ator" "Sally"]}]]
;;    ;; student -> rooms they have class in
;;    (is (= (p/collect [{:let {room :room}
;;                        :extract (p/key= :students)
;;                        _ [student]}]
;;                      {student #{room}}
;;                      m)
;;           {"Eva Lu Ator" #{"West 707" "East 103"}
;;            "Ben Bitdiddle" #{"West 707"}
;;            "Sally" #{"East 103"}}))))
;;
;;(deftest vectors->maps
;;  (let [pieces [{:composer "Bartok"
;;                 :title "Piano Concerto 1"
;;                 :year 1926}
;;                {:composer "Bartok"
;;                 :title "String Quartet 2"
;;                 :year 1917}
;;                {:composer "Ligeti"
;;                 :title "Etude 1"
;;                 :year 1985}]]
;;    (is (= (p/collect [{:let {composer :composer
;;                              :rest rest}}]
;;                      {composer #{rest}}
;;                      pieces)
;;           {"Bartok"
;;            #{{:title "Piano Concerto 1"
;;               :year 1926}
;;              {:title "String Quartet 2"
;;               :year 1917}}
;;            "Ligeti"
;;            #{{:title "Etude 1"
;;               :year 1985}}}))))
