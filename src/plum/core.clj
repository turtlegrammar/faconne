(ns plum.core
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

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

(defn why-i-disallow-certain-destructuring
  [form]
  (str "Form " form " is disallowed because the form {k v} is ambiguous when `k` is a "
       "symbol and `v` is a symbol or collection. For example {k v} is either "
       "destructuring a map, binding `k` to (get m 'v), or it's transform-specific "
       "(doseq through the map, binding each tuple to [k v], building a result). "
       "Because of this ambiguity in certain cases, I've disallowed destructuring maps "
       "with the form {k :key} when the destructuring map has only one entry. If you "
       "want to use this style of destructuring (instead of :keys etc.), then "
       "you need at lest two entries in the map to disambiguate it."))

(defn symbols
  [structure]
  (let [syms (transient #{})]
    (walk/postwalk #(if (symbol? %) (conj! syms %)) structure)
    (persistent! syms)))

(defn parse-domain
  [x]
  (letfn [(parse [x parent-env]
            (cond (symbol? x)
                  {:type :leaf, :bind x,
                   :env (set/union parent-env (symbols x))}

                  (vector? x)
                  (if (> (count x) 1)
                    ;; x is a vector destructuring form
                    {:type :leaf, :bind x,
                     :env (set/union parent-env (symbols x))}

                    ;; x = [s] -- s is non-terminal
                    {:type :seq, :child (parse (first x) parent-env)
                     :env parent-env})

                  (set? x)
                  ;; x = #{s} -- s is non-terminal
                  {:type :seq, :child (parse (first x) parent-env)
                   :env parent-env}

                  (map? x)
                  (if (or ((some-fn :strs :keys :syms) x)
                          (> (count x) 1))
                    {:type :leaf, :bind x, :env (set/union parent-env (symbols x))}
                    (let [[k v] (first x)]
                      (if-not (or (coll? v) (symbol? v))
                        (throw (Exception. (why-i-disallow-certain-destructuring {k v})))
                        (let [this-env (set/union parent-env (symbols k))]
                          {:type :map, :bind k, :child (parse v this-env)
                           :env this-env}))))))]
    (parse x #{})))

(defn maximal-environment
  [parsed-domain]
  (loop [{:keys [child env]} parsed-domain]
    (if-not child env (recur child))))

(defn clause-environments
  [clauses max-env]
  (map (fn [clause] {:req-env (set/intersection max-env (symbols clause))
                     :clause clause})
       clauses))

(defn clauses-in-environment
  [clauses env]
  (group-by #(set/subset? (:req-env %) env) clauses))

(defn add-where-clauses-to-parsed-domain
  [pdomain clauses]
  (let [max-env (maximal-environment pdomain)
        clauses-with-envs (clause-environments clauses max-env)]
    (letfn [(go [eclauses pdomain]
                (let [{:keys [env child]} pdomain

                      {in-env true out-env false}
                      (clauses-in-environment eclauses env)

                      updated-domain (assoc pdomain :where
                                            (mapv :clause in-env))]
                  (if child
                    (update updated-domain :child (partial go out-env))
                    updated-domain)))]
      (go clauses-with-envs pdomain))))

(defn parse-range
  [structure]
  (let [empty-result (cond (map? structure) {}
                           (set? structure) #{}
                           (vector? structure) [])]
    (loop [structure structure
           path []
           type nil
           empty nil
           nest 0]
      (cond (not (coll? structure))
            {:path path :type type
             :leaf structure :empty empty-result
             :nest (dec nest) :empty-leaf-cont empty}

            (map? structure)
            (let [[k v] (first structure)]
              (recur v (conj path k) :map {} (inc nest)))

            (or (vector? structure) (set? structure))
            {:path path
             :leaf (first structure)
             :type (if (= type :map)
                     :seq-in-map
                     :seq)
             :empty-leaf-cont (if (vector? structure) [] #{})
             :empty empty-result
             :nest nest}))))

(defn parse
  [domain range where]
  (let [pdomain (parse-domain domain)
        prange (parse-range range)

        where-domain (add-where-clauses-to-parsed-domain
                      pdomain where)]
    {:domain where-domain
     :range prange}))

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

;; Need A doseq builder [range] that returns a fn action -> clause
;; An "as fn" wrapper that does the structure-sym result-sym persistent stuff
(defn compile
  [domain range where]
  (let [result-sym    (gensym "result")
        structure-sym (gensym "structure")

        {pdomain :domain prange :range}
        (parse domain range where)

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
