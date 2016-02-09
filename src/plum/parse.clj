(ns plum.parse
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

;; Grammar for Domains:
;; Domain := Symbol [terminal]
;;         | Vector
;;         | Set
;;         | Map
;;
;; Vector := [Domain+]
;;
;; Set := #{Domain}
;;
;; Map := {MapEntry+}
;;
;; MapEntry := (:keys | :syms | :strs) [Symbol*]
;;           | :as Symbol
;;           | (Vector Destructure | Symbol | Map Destructure) Domain
;;           | (Keyword | String) Domain
;;           | (:literal ClojureExp) Domain
;;

(defn symbols
  [structure]
  (let [syms (transient #{})]
    (walk/postwalk #(if (symbol? %) (conj! syms %)) structure)
    (persistent! syms)))

(defn parse-domain
  "[{:bind {:type ([:map lvalue]          |
                   [:set]                 |
                   [:vector num_children] |
                   [:literal key]         |
                   [:as lvalue]           |
                   [:leaf lvalue])}
  :env  #{x | x is defined in this binding or in parent bindings}
  :children [parsed domains]}]"
  [dom]
  (letfn [(go [dom parent-env]
              (cond (symbol? dom)
                    [{:bind {:type [:leaf dom]}
                      :env (conj parent-env dom)
                      :children nil}]

                    (or (vector? dom))
                    (let [children (mapcat #(go % parent-env) dom)]
                      [{:bind {:type [:vector (count children)]}
                        :env parent-env
                        :children children}])

                    (set? dom)
                    (if (> (count dom) 1)
                      (throw (Exception. "Sets in the domain can have only one element."))
                      [{:bind {:type [:set]}
                        :env parent-env
                        :children (mapcat #(go % parent-env) dom)}])

                    (map? dom)
                    (for [[k v] dom]
                      (cond (#{:keys :strs :syms} k)
                            {:bind {:type [:leaf {k v}]}
                             :env (set/union parent-env (symbols v))
                             :children nil}

                            (= :as k)
                            {:bind {:type [:as v]}
                             :env (conj parent-env v)
                             :children nil}

                            (or (map? k) (vector? k) (symbol? k))
                            (let [new-env (set/union parent-env (symbols k))]
                              {:bind {:type [:map k]}
                               :env new-env
                               :children (go v new-env)})

                            (list? k)
                            (let [[h t] k
                                  new-env (conj parent-env t)]
                              (if (= h :literal)
                                {:bind {:type [:literal t]}
                                 :env new-env
                                 :children (go v new-env)}
                                (throw (Exception. (str "Unsupported binding type: " h ". "
                                                        "Did you mean `:literal`?")))))

                            (or (keyword? k) (string? k))
                            {:bind {:type [:literal k]}
                             :env parent-env
                             :children (go v parent-env)}))))]
    (go dom #{})))

(defn assign-bind-ids
  [domains]
  (let [id (atom -1)
        get-id #(do (swap! id inc) @id)
        first-id (get-id)]
    (letfn [(go [domain parent-id]
                (let [this-id (get-id)]
                  ;; felt awfully clever when i wrote this
                  (some-> domain
                          (assoc-in [:bind :parent-id] parent-id)
                          (assoc-in [:bind :id] this-id)
                          (update :children (partial mapv #(go % this-id))))))]
      (mapv #(go % first-id) domains))))

(defn squash
  [domains]
  (letfn [(go [domains parent-env]
              (if-not (empty? domains)
                (let [bindings   (mapv :bind domains)
                      joined-env (reduce set/union parent-env
                                         (map :env domains))]
                  {:bindings bindings
                   :env joined-env
                   :child (go (mapcat :children domains) joined-env)})))]
    (go domains #{})))

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
  "Unbelievably ugly. Really need to change this."
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

(def analyze-domain
  "Choose important-sounding words for important functions."
  (comp squash assign-bind-ids parse-domain))

(defn parse
  [domain range where]
  (let [pdomain (analyze-domain domain)
        prange (parse-range range)

        where-domain (add-where-clauses-to-parsed-domain
                      pdomain where)]
    {:domain where-domain
     :range prange}))
