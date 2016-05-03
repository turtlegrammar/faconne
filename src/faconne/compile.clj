(ns faconne.compile
  (:require [faconne.parse :as parse]
            [clojure.core.match :refer [match]]))

(defmacro into!
  [coll xs]
  (loop [xs xs, result coll]
    (if (empty? xs)
      result
      (recur (rest xs) `(conj! ~result ~(first xs))))))

(defn gen-binding
  [{:keys [type parent-id id] :as binding}
   id->sym]
  (let [parent-sym (id->sym parent-id)]
    (match type
           [:map lvalue]
           (let [new-parent (gensym "map-val-parent")]
             {:doseq-bindings [[lvalue new-parent] parent-sym]
              :id->sym {id new-parent}})

           [:proxy]
           (let [new-parent (gensym "proxy-parent")]
             {:let-bindings [new-parent parent-sym]
              :id->sym {id new-parent}})

           [:leaf lvalue]
           {:let-bindings [lvalue parent-sym]}

           [:set]
           (let [new-parent (gensym "set-parent")]
             {:let-bindings [new-parent parent-sym]
              :id->sym {id new-parent}})

           [:vector num-children]
           (if (= 1 num-children)
             (let [new-parent (gensym "vec-parent")]
               {:doseq-bindings [new-parent parent-sym]
                :id->sym {(first id) new-parent}})

             (let [new-parents (for [_ id] (gensym "vec-elem"))]
               {:loop-bindings {:part-size num-children
                                :elem-syms new-parents
                                :vec-sym parent-sym}
                :id->sym (zipmap id new-parents)}))

           [:as lvalue]
           {:let-bindings [lvalue parent-sym]}

           [:literal key]
           (let [new-parent (gensym "literal-parent")]
             {:let-bindings [new-parent `(get ~parent-sym ~key)]
              :id->sym {id new-parent}}))))

(defn gen-bindings
  [bindings id->sym]
  (->> bindings
       (map #(gen-binding % id->sym))
       (apply merge-with into)))

(defn wrap-where-clauses
  [exp clauses]
  (cond (empty? clauses) exp

        (= (count clauses) 1)
        `(when ~(first clauses) ~exp)

        :else `(when (and ~@clauses) ~exp)))

(defn wrap-let-bindings
  [exp let-bindings]
  (if (not-empty let-bindings)
    `(let ~let-bindings ~exp)
    exp))

(defn wrap-doseq-bindings
  [exp doseq-bindings]
  (if (not-empty doseq-bindings)
    `(doseq ~doseq-bindings ~exp)
    exp))

(defn wrap-loop-bindings
  [exp {:keys [part-size elem-syms vec-sym]}]
  (if-not elem-syms
    exp
    (let [as-vec (gensym "as-vec")
          index-sym (gensym "i")
          bindings (->> elem-syms
                        (map-indexed
                         (fn [i elem-sym]
                           [elem-sym `(get ~as-vec (+ ~index-sym ~i))]))
                        (reduce into []))]
      `(let [~as-vec (vec ~vec-sym)
             size# (count ~as-vec)]
         (loop [~index-sym 0]
           (when (< ~index-sym size#)
             (let ~bindings
               ~exp
               (recur (+ ~index-sym ~part-size)))))))))

(defn gen-joiner
  [structure]
  (cond (vector? structure) `into
        (set? structure) `into

        (map? structure)
        (let [[k v] (first structure)]
          (if-not (or (vector? v) (set? v) (map? v))
            `merge
            `(fn [x# y#] (merge-with ~(gen-joiner v) x# y#))))

        :else `(fn [x# _#] x#)))

(defn build-clauses-from-range
  [structure result-sym]
  (cond (or (vector? structure) (set? structure))
        {:init `(volatile! ~(if (vector? structure) `(transient []) `(transient #{})))
         :return `(vswap! ~result-sym persistent!)
         :modifier (if (= (count structure) 1)
                     `(vswap! ~result-sym conj! ~(first structure))
                     `(vswap! ~result-sym (fn [x#] (into! x# ~structure))))}

        (map? structure)
        {:init `(volatile! (transient []))
         :return `(do (vswap! ~result-sym persistent!)
                      (vswap! ~result-sym (fn [x#] (reduce ~(gen-joiner structure) {}  x#))))
         :modifier `(vswap! ~result-sym conj! ~structure)}))

(defn genfn
  [domain range where]
  (let [result-sym    (gensym "result")
        structure-sym (gensym "structure")

        pdomain (parse/parse domain where)

        {inner-modifier-clause :modifier
         return-clause :return
         init-result :init}
        (build-clauses-from-range range result-sym)

        joiner (gen-joiner range)]
    (letfn
        [(go [{:keys [bindings where child] :as domain}
              id->sym]
             (if-not domain
               inner-modifier-clause
               (let [{:keys [let-bindings doseq-bindings
                             loop-bindings id->sym]}
                     (gen-bindings bindings id->sym)]

                 (-> (go child id->sym)
                     (wrap-where-clauses where)
                     (wrap-loop-bindings loop-bindings)
                     (wrap-doseq-bindings doseq-bindings)
                     (wrap-let-bindings let-bindings)))))]

      `(fn [~structure-sym]
         (let [~result-sym ~init-result]
           ~(go pdomain {parse/first-bind-id structure-sym})
           ~return-clause)))))
