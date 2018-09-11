(ns faconne.compile
  (:require [faconne.parse-domain :as domain]
            [faconne.parse-range :as range]
            #?(:clj [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])))

(defn- gen-binding
  "Takes a binding type (domain/parse-domain) and map from binding ids
  to their symbols (domain/assign-bind-ids). Associated with the binding type
  is a parent-sym, which we lookup in the `id->sym` map to get an rvalue to bind
  against."
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
             {:doseq-bindings [new-parent parent-sym]
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

(defn- gen-bindings
  [bindings id->sym]
  (->> bindings
       (map #(gen-binding % id->sym))
       (apply merge-with into)))

(defn- wrap-where-clauses
  [exp clauses]
  (cond (empty? clauses) exp

        (= (count clauses) 1)
        `(when ~(first clauses) ~exp)

        :else `(when (and ~@clauses) ~exp)))

(defn- wrap-let-bindings
  [exp let-bindings]
  (if (not-empty let-bindings)
    `(let ~let-bindings ~exp)
    exp))

(defn- wrap-doseq-bindings
  [exp doseq-bindings]
  (if (not-empty doseq-bindings)
    `(doseq ~doseq-bindings ~exp)
    exp))

(defn- wrap-loop-bindings
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

(defn- build-traverser
  [{:keys [bindings where child] :as domain}
   id->sym
   inner-modifier-clause]
  (if-not domain
    inner-modifier-clause
    (let [{:keys [let-bindings doseq-bindings
                  loop-bindings id->sym]}
          (gen-bindings bindings id->sym)]

      (-> (build-traverser child id->sym inner-modifier-clause)
          (wrap-where-clauses where)
          (wrap-loop-bindings loop-bindings)
          (wrap-doseq-bindings doseq-bindings)
          (wrap-let-bindings let-bindings)))))

(defn gen-iterator
  [domain action where]
  (let [pdomain (domain/parse domain where)
        structure-sym (gensym "structure")]
    `(fn [~structure-sym]
       ~(build-traverser pdomain {domain/first-bind-id structure-sym} action)
       nil)))

(defn gen-transformer
  [domain range where]
  (let [result-sym    (gensym "result")
        structure-sym (gensym "structure")

        pdomain (domain/parse domain where)
        domain-bound-symbols (domain/symbols domain)

        {inner-modifier-clause :modifier
         return-clause         :return
         init-result           :init}
        (range/build-clauses-from-range domain-bound-symbols range result-sym)]

    `(fn [~structure-sym]
       (let [~result-sym ~init-result]
         ~(build-traverser pdomain {domain/first-bind-id structure-sym} inner-modifier-clause)
         ~return-clause))))
