(ns faconne.test.core
  (:require [faconne.core :as f])
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
    (is (= (f/transform m
                        {k1 {k2 v}}
                        {k2 {k1 v}})
           {:b {:a 2
                :c 3}
            :c {:a 5}
            :e {:c 1}}))))

(deftest set-in-map
  (is (= (f/transform profs->classes->students
                      {prof {_ [student]}}
                      {(:name student) #{prof}})
         students->profs))

  (is (= (f/transform profs->classes->students
                      {prof {_ [{:keys [name]}]}}
                      {name #{prof}})
         students->profs))

  (is (= (f/transform profs->classes->students
                      {prof {_ [{:name name}]}}
                      {name #{prof}})
         students->profs)))

(deftest simple-vector-partioning
  (is (-> [1 2 3 4 5 6]
          (f/transform [a _] [a])
          (= [1 3 5])))
  (is (-> [1 2 3 4 5 6]
          (f/transform [a _ c] [a c])
          (= [1 3 4 6])))
  (is (-> [1 2 3 4 5 6]
          (f/transform [_ b _] [b])
          (= [2 5])))
  (is (-> [1 2 3 4 5]
          (f/transform [_ b] [b] :where [b])
          (= [2 4])))
  (is (-> [[1 2] [3 4] [5 6]]
          (f/transform [[a _]] [a])
          (= [1 3 5])))
  (is (-> [[1 2] [3 4] [5 6]]
          (f/transform [[_ b]] [b])
          (= [2 4 6])))
  (is (-> [[1 2] [3 4]]
          (f/transform [[a]] [a])
          (= [1 2 3 4]))))

(deftest complex-vector-partitioning
  (is (-> {{:k :a} [1 2 3 4 5 6]
           {:k :b} [7 8 9 10 11 12]}
          (f/transform {{:keys [k]} [a _ c]} {(+ a c) k}
                       :where [(even? a) (even? c)])
          (= {10 :a, 22 :b})))

  (is (-> [[1 2 3] [4 5 98 7] [5 6 8 17] [13 14 15]]
          (f/transform [[_ b] _] [b]
                       :where [b (even? b)])
          (= [2 6])))

  (is (-> [[1 2 3] [4 5 98 7] [5 6 8 17] [13 14 15]]
          (f/transform [_ [a _]] [a]
                       :where [a (even? a)])
          (= [4 98])))

  (is (-> [[1 2 3] [4 5 98 7] [5 6 8 17] [13 14 15]]
          (f/transform [_ [_ b]] [b]
                       :where [b (even? b)])
          (= [14])))

  (is (-> [[1 2 3] [4 5 98 7] [5 6 8 17] [13 14 15]]
          (f/transform [[a _] _] [a]
                       :where [a (even? a)])
          (= [8]))))

(deftest key-literals
  (let [data [{:num 6
               :coll [1 2]}
              {:num 7
               :coll [-94 -100]}
              {:num 8
               :coll [3 4]}]]
    (is (-> data
            (f/transform [{:num n :coll [x]}] #{x}
                         :where [(even? n)])
            (= #{1 2 3 4})))
    (is (-> data
            (f/transform [{:keys [num] :coll [x]}] #{x}
                         :where [(odd? num)])
            (= #{-94 -100}))))

  (let [pieces
        [{:composer "Bartók" :title "Piano Concerto 1" :year 1926}
         {:composer "Bartók" :title "String Quartet 2" :year 1917}
         {:composer "Ligeti" :title "Etude 1" :year 1985}
         {:composer "Ligeti" :title "Mysteries of the Macabre" :year 1992}]

        by-composer-and-year
        {"Bartók"
         {1926 [{:composer "Bartók", :title "Piano Concerto 1", :year 1926}],
          1917 [{:composer "Bartók", :title "String Quartet 2", :year 1917}]},
         "Ligeti"
         {1985 [{:composer "Ligeti", :title "Etude 1", :year 1985}],
          1992 [{:composer "Ligeti", :title "Mysteries of the Macabre", :year 1992}]}}]
    (is (-> pieces
            (f/transform [{:keys [composer year] :as piece}] {composer {year [piece]}})
            (= by-composer-and-year)))
    (is (-> pieces
            (f/transform [{:keys [composer] :year y :as piece}] {composer {y [piece]}})
            (= by-composer-and-year)))
    (is (-> pieces
            (f/transform [{:composer c :year y :as piece}] {c {y [piece]}})
            (= by-composer-and-year))))

  (let [data [{:a 1 "b" 2}, {:a 2 "b" 3},
              {:a 3 "b" 5}, {:a 4 "b" 4}]
        result {1 2 2 3 4 4 3 5}]
    (is (-> data
            (f/transform [{:a a "b" b}] {a b})
            (= result)))
    (is (-> data
            (f/transform [{:keys [a] "b" b}] {a b})
            (= result)))
    (is (-> data
            (f/transform [{:strs [b] :a a}] {a b})
            (= result))))

  (let [data [{:keys 1 :strs 2 :syms 3 :as 4}
              {:keys 5 :strs 6 :syms 7 :as 8}]]
    (is (-> data
            (f/transform [{(:literal :keys) a
                           (:literal :strs) b
                           (:literal :syms) c
                           (:literal :as)   d}]
                         #{(+ a b c d)})
            (= #{10 26}))))

  (is (-> {[101 23] 4, [98] 2}
          (f/transform {(:literal [101 23]) x} #{x})
          (= #{4}))))

(deftest variable-key-literal
  (let [merge-key-vals (fn [m k1 k2]
                         (f/transform m
                                      {(:literal k1) [v1]
                                       (:literal k2) [v2]}
                                      #{v1 v2}))]
    (is (-> {:a [1 2 3], :b [3 4 5], :c [5 6 7]}
            (merge-key-vals :a :b)
            (= #{1 2 3 4 5})))))


(deftest where
  (is (= {1 2, 3 4}
         (f/transform {1 (range), 2 [1, 2], 3 (range), 4 [3, 4]}
                      {k [n]} {n k}
                      :where [(even? k) (odd? n)]))))

(deftest key-destructuring
  (let [pair-map {[1 2] 3 [4 5] 6}
        map-map {{:a 1 :b 2} 3 {:a 4 :b 5} 6}]
    (is (-> pair-map
            (f/transform {[n1 n2] v} #{(+ n1 n2 v)})
            (= #{6 15})))
    (is (-> map-map
            (f/transform {{:keys [a b]} v} #{(+ a b v)})
            (= #{6 15})))))

(deftest combinations
  (is (= #{#{:c :d} #{:e :a} #{:b :a}}
         (f/transform {:a 7, :b 3, :c 5, :d 5, :e 3}
                      {k v, k' v'}
                      #{#{k k'}}
                      :where [(not= k k') (= 10 (+ v v'))]))))


(deftest higher-level-sanity-tests
  (let [json [{:store-name "Tom's Records"
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
                        :quantity 2}]}]

        transform (f/transformer [{:store-name store
                                   :location loc
                                   :stock [{:keys [artist title quantity]}]}]
                                 {artist {title [[(str store " @ " loc) quantity]]}})
        result {"Bartók"
                {"String Quartets"
                 [["Tom's Records @ 1234 Main Street" 5]]},

                "Ligeti"
                {"Violin Concerto"
                 [["Tom's Records @ 1234 Main Street" 1]
                  ["Roger's Records @ 789 Secondary Street" 3]]},

                "Scriabin"
                {"12 Etudes"
                 [["Roger's Records @ 789 Secondary Street" 2]]}}]
    (is (= result (transform json)))))
