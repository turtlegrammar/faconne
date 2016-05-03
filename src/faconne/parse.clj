(ns faconne.parse
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.core.match :refer [match]]))

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

(defn- symbols
  "Get a set of all the symbols used in a data structure."
  [structure]
  (let [syms (transient #{})]
    (walk/postwalk #(if (symbol? %) (conj! syms %)) structure)
    (persistent! syms)))

(defn- parse-domain
  "Returns:
  [{:bind {:type  ([:map lvalue]          |
                   [:proxy]               |
                   [:set]                 |
                   [:vector num-children] |
                   [:literal key]         |
                   [:as lvalue]           |
                   [:leaf lvalue])}
  :env  #{x | sym x is defined in this binding or in parent bindings}
  :children [parsed domains]}]"
  [dom]
  (letfn [(go [dom parent-env]
              (cond (symbol? dom)
                    [{:bind {:type [:leaf dom]}
                      :env (conj parent-env dom)
                      :children nil}]

                    ;; Parsing vectors is tricky because for a form like
                    ;; [s1 ... sn], the children of each si must be linked to si
                    ;; so that their constituents can be bound against si.
                    ;; For example, in [[a] [b]], each b must be bound against the
                    ;; odd-indexed elements of the outer vector.
                    ;; If we just (mapcat go children), then this information is lost.
                    ;; So, each element in the vector is given a type of :proxy,
                    ;; which is just a way to preserve the separate 'bloodlines'
                    ;; of each elements' children.
                    (or (vector? dom))
                    (let [children (map #(go % parent-env) dom)
                          proxied-children (mapcat (fn [c]
                                                     (if (> (count c) 1)
                                                       [{:bind {:type [:proxy]}
                                                          :env parent-env
                                                          :children c}]
                                                       c))
                                                   children)]
                      [{:bind {:type [:vector (count children)]}
                        :env parent-env
                        :children proxied-children}])

                    (set? dom)
                    (if (> (count dom) 1)
                      (throw (Exception. "Sets in the domain can have only one element."))
                      [{:bind {:type [:set]}
                        :env parent-env
                        :children (go (first dom) parent-env)}])

                    (map? dom)
                    (for [[k v] dom]
                      (cond (#{:keys :strs :syms} k)
                            (if-not (and (vector? v) (every? symbol? v))
                              (throw (Exception. (str "The binding " {k v} " is expected to "
                                                      "conform to Clojure's map destructuring "
                                                      "syntax; the value must be a vector of "
                                                      "symbols. If you want to treat the map "
                                                      "as having " k " as a key, then use "
                                                      {'(:literal k) v})))
                              {:bind {:type [:leaf {k v}]}
                               :env (set/union parent-env (symbols v))
                               :children nil})

                            (= :as k)
                            (if-not (symbol? v)
                              (throw (Exception. (str "The binding {:as v} means that the "
                                                      "entire map should be bound to v. "
                                                      "If you mean to use `:as` as a key literal, "
                                                      "then use: " {'(:literal :as) v} ".")))
                              {:bind {:type [:as v]}
                               :env (conj parent-env v)
                               :children nil})

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
                                (throw (Exception. (str "Unsupported binding type: " k ". "
                                                        "You probably meant: " {'(:literal k) v})))))

                            (or (keyword? k) (string? k))
                            {:bind {:type [:literal k]}
                             :env parent-env
                             :children (go v parent-env)}))))]
    (go dom #{})))

(defn- assign-bind-ids
  "parse-domain returns an unwieldy multiway tree: each domain 'frame' has
  exactly one binding and n (variable) children. It would be easier to work with if each frame
  had instead n bindings and 1 child. However, each binding must know its parent.
  By assigning each binding an `:id` and `:parent-id`, we can later squash all of them into
  a simple linear list (see `squash`)."
  [domains]
  (let [id (atom -1)
        get-id #(do (swap! id inc) @id)
        first-id (get-id)]
    (letfn [(update-domain [domain parent-id this-id new-children]
              (-> domain
                  (assoc-in [:bind :parent-id] parent-id)
                  (assoc-in [:bind :id] this-id)
                  (assoc :children new-children)))
            (go [domain parent-id]
                (if-let [type (-> domain :bind :type)]
                  (match type
                         ;; Elements of the vector must have distinct parent ids.
                         [:vector num-children]
                         (let [these-ids (mapv (fn [_] (get-id)) (range num-children))
                               assigned-children (->> these-ids
                                                      (mapv vector (:children domain))
                                                      (mapv (partial apply go)))]
                           (update-domain domain parent-id these-ids assigned-children))

                         :else
                         (let [this-id (get-id)
                               new-children (mapv #(go % this-id)
                                                  (:children domain))]
                           (update-domain domain parent-id this-id new-children)))))]

      (mapv #(go % first-id) domains))))

(def first-bind-id 0)

(defn- squash
  "Squash a vector of binding frames - each having exactly one binding
  and multiple children - into a single binding frame having multiple
  bindings and exactly one child."
  [domains]
  (letfn [(go [domains parent-env]
              (if-not (empty? domains)
                (let [bindings   (mapv :bind domains)
                      joined-env (reduce set/union parent-env
                                         (mapv :env domains))]
                  {:bindings bindings
                   :env joined-env
                   :child (go (mapcat :children domains) joined-env)})))]
    (go domains #{})))

(defn- maximal-environment
  "Return the environment containing every symbol used in the domain."
  [{:keys [child env]}]
  (if-not child env (recur child)))

(defn- clause-environments
  [clauses max-env]
  (for [clause clauses]
    {:req-env (set/intersection max-env (symbols clause))
     :clause clause}))

(defn- clauses-in-environment
  [clauses env]
  (group-by #(set/subset? (:req-env %) env) clauses))

(defn- add-where-clauses
  "Associate where clauses with the highest possible frame in the domain."
  [domain clauses]
  (let [max-env (maximal-environment domain)
        clauses-with-envs (clause-environments clauses max-env)]
    (letfn [(go [eclauses {:keys [env] :as domain}]
                (if domain
                  (let [{in-env true out-env false}
                        (clauses-in-environment eclauses env)]
                    (-> domain
                        (assoc :where (mapv :clause in-env))
                        (update :child (partial go out-env))))))]
      (go clauses-with-envs domain))))

(defn- validate-where-clauses
  [clauses]
  (if-not (or (nil? clauses) (vector? clauses))
    (throw (Exception. "The arguments to `:where` should be a vector of Clojure expressions."))
    clauses))

(def ^:private analyze-domain
  "Choose important-sounding words for important functions."
  (comp squash assign-bind-ids parse-domain))

(defn parse
  [domain where]
  (let [pdomain (analyze-domain domain)]
    (->> where
         validate-where-clauses
         (add-where-clauses pdomain))))
