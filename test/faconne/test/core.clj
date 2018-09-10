(ns faconne.test.core
  (:require [faconne.core :as f])
  (:use [clojure.test]))

(def test-times 20)

(defn gen-structure-from-domain
  "Create a function that when invoked will generate a random extension of `domain`."
  [domain]
  (let [max-size 10
        get-size (fn [& args] (Math/floor (rand max-size)))
        exec     (fn [f] (f))]
    (letfn [(create [domain]
              (cond (symbol? domain) rand

                    (or (vector? domain) (set? domain))
                    (let [create-elems (map create domain)
                          return (if (vector? domain) identity (partial into #{}))]
                      (fn [& args]
                        (->> (for [_ (range 0 (* (count domain) (get-size)))]
                               (mapv exec create-elems))
                             (reduce into [])
                             return)))

                    (map? domain)
                    (let [[k v] (first domain)
                          create-key (create k)
                          create-val (create v)]
                      (fn [& args]
                        (->> (for [_ (range 0 (get-size))] [(create-key) (create-val)])
                             (into {}))))))]
      (create domain))))

(defn test-trans
  [mkdata domain range' where hand-written]
  (let [data          (mkdata)
        from-trans-fn (eval `(f/transformer ~domain ~range' :where ~where))
        from-trans    (from-trans-fn data)
        from-hand     (hand-written data)]
      (testing (str "Testing transformer on " domain " " range' " " where
                    ".\n Data = " data)
        (is (= from-trans from-hand)))))

(defmacro test-transformer
  [domain range' where hand-written]
  `(let [mkdata# (gen-structure-from-domain (quote ~domain))]
     (doseq [_# (range 0 ~test-times)]
       (test-trans mkdata# (quote ~domain) (quote ~range') (quote ~where) ~hand-written))))

(deftest test-map-domains
  (let [swap-key-order ;; {k1 {k2 v}} -> {k2 {k1 v}}
        (fn [m]
          (or
           (apply merge-with merge
                  (map (fn [[k1 inner]]
                         (apply merge-with merge
                                (map (fn [[k2 v]]
                                       {k2 {k1 v}}) inner))) m))
           {}))
        remove-inner ;; {k1 {k2 v}} -> {k1 #{v}}
        (fn [m]
          (or
           (apply merge-with into
                  (map (fn [[k inner]]
                         (apply merge-with into
                                (map (fn [[_ v]] {k #{v}}) inner)))
                       m))
           {}))

        flip (fn [m] (or (into {} (map (fn [[k v]] [v k]) m)) {})) ;; {k v} -> {v k}

        skipping-flatset (fn [m] ;; {k [v]} -> #{[k v]}
                            (or
                             (reduce into #{}
                                     (map (fn [[k vector]]
                                            (reduce into #{} (->> vector
                                                                  (partition 2)
                                                                  (map first)
                                                                  (map (fn [v] #{[k v]})))))
                                          m))
                             {}))
        sums-of-all-pairs-of-vals (fn [m] (let [vs (vals m)]
                                            (reduce into #{}
                                                    (map (fn [i]
                                                           (into #{} (map (fn [j] (+ i j)) vs)))
                                                         vs))))]
    (test-transformer {k1 {k2 v}} {k2 {k1 v}} [] swap-key-order)
    (test-transformer {k {_ v}} {k #{v}} [] remove-inner)
    (test-transformer {k v} {v k} [] flip)
    (test-transformer {k [v _]} #{[k v]} [] skipping-flatset)
    (test-transformer {k1 v1, k2 v2} #{(+ v1 v2)} [] sums-of-all-pairs-of-vals)))

(deftest test-vector-domains
  (let [seconds (fn [v] (map second (partition 2 v)))
        sums-of-pairs-of-odds (fn [v] (map (fn [[a b c d]] (+ a c)) (partition 4 v)))
        sums-of-1-3-in-2 (fn [v] (->> v
                                      (partition 3)
                                      (map second)
                                      (map (partial partition 3))
                                      (map (fn [v2] (into #{}
                                                          (map (fn [[a _ c]] (+ a (or c 0))) v2))))
                                      (reduce into #{})))
        super-contrived (fn [v] (->> v
                                     (partition 2)
                                     (map first)
                                     (map #(->> %
                                                (map (fn [[k vector]]
                                                       (into #{} (map (partial + k) vector))))
                                                (reduce into #{})))
                                     (reduce into #{})))]
    (test-transformer [_ b] [b] [] seconds)
    (test-transformer [a _ c _] [(+ a c)] [a c] sums-of-pairs-of-odds)
    (test-transformer [[a]] [a] [] (partial reduce into []))
    (test-transformer [_ [a _ c] _] #{(+ a c)} [a c] sums-of-1-3-in-2)
    (test-transformer [{k [v]} _] #{(+ k v)} [] super-contrived)))

(deftest test-set-domains
  (let [adj-sums (fn [s] (->> s
                              (map (fn [v] (into #{} (map (partial apply +) (partition 2 v)))))
                              (reduce into #{})))]
    (test-transformer #{[a b]} #{(+ a b)} [a b] adj-sums)))

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
  (let [profs->classes->students
        {"Sussman" {"AI" [{:name "John" :grade "A"}
                          {:name "Sally" :grade "B"}]
                    "Compilers" [{:name "Tom" :grade "B"}
                                 {:name "John" :grade "B"}]}
         "Abelson" {"Machine Learning" [{:name "Sally" :grade "C"}
                                        {:name "Tom" :grade "B-"}]
                    "Compilers" [{:name "Eva Lu Ator" :grade "B"}
                                 {:name "Ben Bitdiddle" :grade "A"}]}}

        students->profs {"John" #{"Sussman"} "Sally" #{"Abelson" "Sussman"}
                         "Tom" #{"Abelson" "Sussman"} "Eva Lu Ator" #{"Abelson"}
                         "Ben Bitdiddle" #{"Abelson"}}]

    ;; todo: fix
    ;; (is (= (f/transform profs->classes->students
    ;;                     {prof {_ [student]}}
    ;;                     {(:name student) #{prof}})
    ;;        students->profs))

    (is (= (f/transform profs->classes->students
                        {prof {_ [{:keys [name]}]}}
                        {name #{prof}})
           students->profs))

    (is (= (f/transform profs->classes->students
                        {prof {_ [{:name name}]}}
                        {name #{prof}})
           students->profs))))

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

;;;;;;;;;;;;;;
;; Reducers ;;
;;;;;;;;;;;;;;

(deftest simple-reducers
  (is (= (f/transform [1 2 3 1 2 3] [x] (apply max [x]))
         3))
  (is (= (f/transform [1 2 3 1 2 3] [x] (apply max [x (inc x)]))
         4))
  (is (= (f/transform [1 2 3 1 2 3] [x] [x (count [x])])
         [1 6 2 6 3 6 1 6 2 6 3 6]))
  (is (= (f/transform [1 2 3 1 2 3] [x] (apply max [x (count [x])]))
         6))
  (is (= (f/transform [1 2 3 1 2 3] [x] (count #{x}))
         3))
  (is (= (f/transform {:a [1 2 3],
                       :b [8 9 5],
                       :d [4 5 6]}
                      {k [v]}
                      #{(apply max [v])})
         #{9}))

  ;; readme
  (is (= (f/transform {:a [1 2 3],
                       :b [8 9 5],
                       :d [4 5 6]}
                      {k [v]}
                      (apply max [v]))
         9))



  )

(deftest complicated-reducers
  (let [student-data
        [{:student "john", :grade1 97, :grade2 89, :course "math", :campus "east"}
         {:student "john", :grade1 90, :grade2 70, :course "english", :campus "east"}
         {:student "john", :grade1 70, :grade2 80, :course "history", :campus "east"}
         {:student "dave", :grade1 80, :grade2 80, :course "math", :campus "east"}
         {:student "dave", :grade1 100, :grade2 90, :course "english", :campus "east"}
         {:student "mary", :grade1 90, :grade2 86, :course "math", :campus "west"}
         {:student "mary", :grade1 92, :grade2 81, :course "english", :campus "west"}
         {:student "mary", :grade1 94, :grade2 83, :course "history", :campus "west"}]

        average (fn [xs] (/ (reduce + 0 xs) (count xs)))]

    (is (= (f/transform student-data
                        [{:keys [student grade1 grade2 course]}]
                        {student (apply max [grade2])})
           {"john" 89, "dave" 90, "mary" 86}))

    (is (= (f/transform student-data
                        [{:keys [student grade1 grade2 course]}]
                        {student (max (apply max [grade1])
                                      (apply max [grade2]))})
           {"john" 97, "dave" 100, "mary" 94}))

    (is (= (f/transform student-data
                        [{:keys [student grade1 grade2 course] :as tuple}]
                        {student (:course (apply max-key
                                                 :grade
                                                 [{:grade (/ (+ grade1 grade2) 2),
                                                   :course course}]))})
           {"john" "math", "dave" "english", "mary" "history"}))

    (is (= (f/transform student-data
                        [{:keys [student grade1 grade2 course]}]
                        {course (count [student])}
                        :where [(> grade1 95)])
           {"math" 1, "english" 1}))

    ;;readme
    (is (= (f/transform student-data
                        [{:keys [student grade1 grade2 course campus]}]
                        {campus {:number-students (count #{student})
                                 :avg-grade-per-course {course (average [grade1])}
                                 :student-stats {student {course grade1}}}})
           {"east"
            {:number-students 2,
             :avg-grade-per-course {"math" 177/2,
                                    "english" 95,
                                    "history" 70},
             :student-stats {"john" {"math" 97,
                                     "english" 90,
                                     "history" 70},
                             "dave" {"math" 80,
                                     "english" 100}}}
            "west"
            {:number-students 1,
             :avg-grade-per-course {"math" 90,
                                    "english" 92,
                                    "history" 94},
             :student-stats {"mary" {"math" 90,
                                     "english" 92,
                                     "history" 94}}}}))))
