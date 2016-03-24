(ns faconne.compile
  (:require [faconne.parse :as parse]
            [clojure.core.match :refer [match]]))

(defn create-assoc!
  [m k v]
  (assoc! (or m (transient {})) k v))

(defn assoc-in!
  [m [k & ks] v]
  (if-not ks
    (create-assoc! m k v)
    (create-assoc! m k (assoc-in! (get m k) ks v))))

(defn update!
  [m k f]
  (create-assoc! m k (f (get m k))))

(defn update-in!
  [m [k & ks] f]
  (if-not ks
    (update! m k f)
    (create-assoc! m k (update-in! (get m k) ks f))))

;; Could be a function, but `xs` is known at compile time.
;; By the same reasoning, update-in! and assoc-in! could be macros.
;; For some reason, they're not, and this is.
(defmacro into!
  [coll xs default]
  `(do ~@(map-indexed (fn [i x] (if (> i 0)
                                  `(conj! ~coll ~x)
                                  `((fnil conj! ~default) ~coll ~x)))
                      xs)))

(defn deep-persistent!
  [nest coll]
  (if (= nest 0)
    (persistent! coll)
    (->> (persistent! coll)
         (mapv (fn [[k v]] [k (deep-persistent! (dec nest) v)]))
         (into {}))))

(defn modifier-clause
  [{:keys [path leaf type empty empty-leaf-cont]}
   result-sym]
  (case type
    :seq `(into! ~result-sym ~leaf ~empty)

    :map `(assoc-in! ~result-sym ~path ~leaf)

    :seq-in-map `(update-in! ~result-sym ~path
                             (fn [x#] (into! x# ~leaf ~empty-leaf-cont)))))

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
    (let [index-sym (gensym "i")
          bindings (->> elem-syms
                        (map-indexed
                         (fn [i elem-sym]
                           [elem-sym `(get ~vec-sym (+ ~index-sym ~i))]))
                        (reduce into []))]
      `(let [size# (count ~vec-sym)]
         (loop [~index-sym 0]
           (when (< ~index-sym size#)
             (let ~bindings
               ~exp
               (recur (+ ~index-sym ~part-size)))))))))

(defn genfn
  [domain range where]
  (let [result-sym    (gensym "result")
        structure-sym (gensym "structure")

        {pdomain :domain prange :range}
        (parse/parse domain range where)

        modifier      (modifier-clause prange result-sym)
        empty-result  (:empty prange)
        nest          (:nest prange)]
    (letfn
        [(go [{:keys [bindings where child] :as domain}
              id->sym]
             (if-not domain
               modifier
               (let [{:keys [let-bindings doseq-bindings
                             loop-bindings id->sym]}
                     (gen-bindings bindings id->sym)]

                 (-> (go child id->sym)
                     (wrap-where-clauses where)
                     (wrap-loop-bindings loop-bindings)
                     (wrap-let-bindings let-bindings)
                     (wrap-doseq-bindings doseq-bindings)))))]

      `(fn [~structure-sym]
         (let [~result-sym (transient ~empty-result)]
           ;; need to remove hard-coded 0
           ~(go pdomain {0 structure-sym})
           (deep-persistent! ~nest ~result-sym))))))
