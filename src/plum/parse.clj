(ns plum.parse
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

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

(declare parse-domain-map)
(declare parse-domain-vector)
(declare parse-domain-set)
(defn parse-domain
  "{:type => (:leaf | :seq | :map)
  :env => #{s | s is bound in this frame (or in parents)}
  :bind => [{:type (:destructure | :literal)
            :lvalue
            :key (only if :type = :literal)}]
  :child => parsed child}"
  [x]
  (letfn [(parse [x parent-env]
            (cond (symbol? x)
                  {:type :leaf
                   :bind [{:type :destructure
                            :lvalue x}]
                   :env (set/union parent-env (symbols x))}

                  (vector? x)
                  (if (> (count x) 1)
                    ;; x is a vector destructuring form
                    {:type :leaf
                     :bind [{:type :destructure
                              :value x}]
                     :env (set/union parent-env (symbols x))}

                    ;; x = [s] -- s is non-terminal
                    {:type :seq
                     :child (parse (first x) parent-env)
                     :env parent-env})

                  (set? x)
                  ;; x = #{s} -- s is non-terminal
                  {:type :seq
                   :child (parse (first x) parent-env)
                   :env parent-env}

                  (map? x)
                  (parse-domain-map x parent-env)
                  ))]
    (parse x #{})))

(defn- parse-domain-map
  [m parent-env]
  (let [x (loop [m m
                 env #{}
                 type nil
                 bind []
                 children []]

            (let [[[k v] & others] m]
              (cond (#{:strs :keys :syms} k)
                    (recur others
                           (set/union env (symbols v))
                           (or type :leaf)
                           (conj bind {:type :destructure
                                       :lvalue {k v}})
                           children)

                    (and (not coll? k) (not symbol? k))
                    (recur others
                           (set/union env (symbols v))
                           ;; this prevents doseqqing over a key literal's value
                           (or type :leaf)
                           (conj bind {:type :literal
                                       :lvalue v
                                       :key k})
                           (conj children))

                    :else
                    (recur others
                           (set/union env (symbols k))
                           :map
                           (conj bind {:type :destructure
                                       :lvalue k})
                           (conj children v)))))])
  (if (or ((some-fn :strs :keys :syms) x)
          (> (count x) 1))
    {:type :leaf, :bind x, :env (set/union parent-env (symbols x))}
    (let [[k v] (first x)]
      (if-not (or (coll? v) (symbol? v))
        (throw (Exception. (why-i-disallow-certain-destructuring {k v})))
        (let [this-env (set/union parent-env (symbols k))]
          {:type :map, :bind k, :child (parse v this-env)
           :env this-env})))))

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
