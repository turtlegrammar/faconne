(ns plum.core
  (:require [plum.parse :as parse]))

(defn create-assoc!
  [m k v]
  (assoc! (or m (transient {})) k v))

(defn assoc-in!
  [m [k & ks] v]
  (if-not ks
    (create-assoc! m k v)
    (create-assoc! m k (assoc-in! (get m k) ks v))))

(defn update!
  "Hello, is this update?
  No, this is update!.
  Update? How are you doing?
  I told you, this is update!."
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

(defn wrap-where-clauses
  [exp clauses]
  (cond (empty? clauses) exp

        (= (count clauses) 1)
        `(when ~(first clauses) ~exp)

        :else `(when (and ~@clauses) ~exp)))

;; Need A doseq builder [range] that returns a fn action -> clause
;; An "as fn" wrapper that does the structure-sym result-sym persistent stuff
(defn compile
  [domain range where]
  (let [result-sym    (gensym "result")
        structure-sym (gensym "structure")

        {pdomain :domain prange :range}
        (parse/parse domain range where)

        modifier      (modifier-clause prange result-sym)
        empty-result  (:empty prange)
        nest          (-> prange :nest)]
    (letfn
        [(go [{:keys [type bind child leaf where]} ;; parsed domain
              struct-sym]
             (let [next-sym (gensym "val")]
               (case type
                 :map
                 `(doseq [[~bind ~next-sym] ~struct-sym]
                    ~(wrap-where-clauses (go child next-sym)
                                         where))

                 :seq
                 `(doseq [~next-sym ~struct-sym]
                    ~(wrap-where-clauses (go child next-sym)
                                         where))

                 :leaf
                 `(let [~bind ~struct-sym]
                    ~(wrap-where-clauses modifier
                                         where)))))]
      `(fn [~structure-sym]
         (let [~result-sym (transient ~empty-result)]
           ~(go pdomain structure-sym)
           (deep-persistent! ~nest ~result-sym))))))

(defmacro transformer
  [domain range & options]
  (let [where (:where (apply hash-map options))]
    (compile domain range where)))

(defmacro transform
  [x domain range & options]
  `(let [f# (transformer ~domain ~range ~@options)]
     (f# ~x)))

(defmacro print-generated-fn
  [domain range & options]
  (let [where (:where (apply hash-map options))]
    (clojure.pprint/pprint (compile domain range where))))
