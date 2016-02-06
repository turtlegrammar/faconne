(ns plum.test.implementation-details
  (:require [plum.core :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse
  (let [exp '{{:keys [x y]} {k2 {v [p1 p2]}}}
        where-clauses '[(= x y) (= v x) (= p1 k2)]]
    (is (= (add-where-clauses-to-parsed-domain (parse-domain exp) where-clauses)
           {:type :map,
            :bind '{:keys [x y]},
            :child
            {:type :map,
             :bind 'k2,
             :child
             {:type :map,
              :bind 'v,
              :child
              {:type :leaf,
               :bind '[p1 p2],
               :env '#{x k2 y p2 v p1},
               :where '[(= p1 k2)]},
              :env '#{x k2 y v},
              :where '[(= v x)]},
             :env '#{x k2 y},
             :where '[]},
            :env '#{x y},
            :where '[(= x y)]}))))
