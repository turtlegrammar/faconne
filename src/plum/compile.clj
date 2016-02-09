(ns plum.compile
  (:require [plum.parse :as parse]
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
  "My name is update!
  Hello update, it's been a while. How are you?
  No, my name is update!
  Long time no see, update!
  You mean update!!"
  [m k f]
  (create-assoc! m k (f (get m k))))

(defn update-in!
  [m [k & ks] f]
  (if-not ks
    (update! m k f)
    (create-assoc! m k (update-in! (get m k) ks f))))

(defn deep-persistent!
  [nest coll]
  (if (= nest 0)
    (persistent! coll)
    (->> (persistent! coll)
         (map (fn [[k v]] [k (deep-persistent! (dec nest) v)]))
         (into {}))))

(defn modifier-clause
  [{:keys [path leaf type empty empty-leaf-cont]}
   result-sym]
  (case type
    :seq `((fnil conj! (transient ~empty)) ~result-sym ~leaf)

    :map `(assoc-in! ~result-sym ~path ~leaf)

    :seq-in-map `(update-in! ~result-sym ~path
                             (fn [x#] ((fnil conj! (transient ~empty-leaf-cont))
                                       x# ~leaf)))))

(defn wrap-where-clauses
  [exp clauses]
  (cond (empty? clauses) exp

        (= (count clauses) 1)
        `(when ~(first clauses) ~exp)

        :else `(when (and ~@clauses) ~exp)))

(defn gen-bindings
  [bindings struct-sym-map]
  (loop [let-bindings []
         doseq-bindings []
         loop-bindings {} ;; at most one per level
         [binding & rest-bindings] bindings
         struct-sym-map struct-sym-map]
    (if-not binding
      {:let-bindings let-bindings
       :doseq-bindings doseq-bindings
       :loop-bindings loop-bindings
       :struct-sym-map struct-sym-map}
      (let [{:keys [type parent-id id]} binding
            parent-sym (struct-sym-map parent-id)]
        (match type

               [:map lvalue]
               (let [new-parent (gensym "map-val-parent")]
                 (recur let-bindings
                        (conj doseq-bindings [lvalue new-parent] parent-sym)
                        loop-bindings
                        rest-bindings
                        (assoc struct-sym-map id new-parent)))

               [:leaf lvalue]
               (recur (conj let-bindings lvalue parent-sym)
                      doseq-bindings
                      loop-bindings
                      rest-bindings
                      struct-sym-map)

               [:set]
               (let [new-parent (gensym "set-parent")]
                 (recur let-bindings
                        (conj doseq-bindings new-parent parent-sym)
                        loop-bindings
                        rest-bindings
                        (assoc struct-sym-map id new-parent)))

               [:vector num-children]
               (if (= 1 num-children)
                 (let [new-parent (gensym "vec-parent")]
                   (recur let-bindings
                          (conj doseq-bindings new-parent parent-sym)
                          loop-bindings
                          rest-bindings
                          (assoc struct-sym-map (first id) new-parent)))

                 (let [new-parents (for [_ id] (gensym "vec-elem"))
                       new-sym-map (reduce (fn [acc [this-id sym]]
                                             (assoc acc this-id sym))
                                           struct-sym-map
                                           (map vector id new-parents))]
                   (recur let-bindings
                          doseq-bindings
                          {:part-size num-children
                           :elem-syms new-parents
                           :vec-sym parent-sym}
                          rest-bindings
                          new-sym-map)))

               [:as lvalue]
               (recur (conj let-bindings lvalue parent-sym)
                      doseq-bindings
                      loop-bindings
                      rest-bindings
                      struct-sym-map)

               [:literal key]
               (let [new-parent (gensym "literal-parent")]
                 (recur (conj let-bindings new-parent `(get ~parent-sym ~key))
                        doseq-bindings
                        loop-bindings
                        rest-bindings
                        (assoc struct-sym-map id new-parent))))))))

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

(defn compile
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
              struct-sym-map]
             (if-not domain
               modifier
               (let [{:keys [let-bindings doseq-bindings
                             loop-bindings struct-sym-map]}
                     (gen-bindings bindings struct-sym-map)]

                 (-> (go child struct-sym-map)
                     (wrap-loop-bindings loop-bindings)
                     (wrap-doseq-bindings doseq-bindings)
                     (wrap-let-bindings let-bindings)
                     (wrap-where-clauses where)))))]


      `(fn [~structure-sym]
         (let [~result-sym (transient ~empty-result)]
           ;; need to remove hard-coded 0
           ~(go pdomain {0 structure-sym})
           (deep-persistent! ~nest ~result-sym))))))
