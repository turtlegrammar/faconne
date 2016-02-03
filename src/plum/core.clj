(ns plum.core
  (:require [clojure.set :as set]))

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

(defn deep-persistent!
  [nest coll]
  (if (= nest 0)
    (persistent! coll)
    (->> (persistent! coll)
         (map (fn [[k v]] [k (deep-persistent! (dec nest) v)]))
         (into {}))))

(defn why-i-disallow-certain-destructuring
  [form]
  (str "Form " form " is disallowed because the form {k v} is ambiguous when k is a "
       "symbol and v is a symbol or collection. For example {k v} is either "
       "destructuring a map, binding `k` to (get m 'v), or it's transform-specific "
       "(doseq through the map, binding each tuple to [k v], building a result). "
       "Because of this ambiguity in certain cases, I've disallowed destructuring maps "
       "with the form {k :key} when the destructuring map has only one entry. If you "
       "want to use this style of destructuring (instead of :keys etc.), then "
       "you need at lest two entries in the map to disambiguate it."))

(defn parse-domain
  [x]
  (cond (symbol? x)
        {:type :leaf, :bind x}

        (vector? x)
        (if (> (count x) 1)
          ;; x is a vector destructuring form
          {:type :leaf, :bind x}

          ;; x = [s] -- s is non-terminal
          {:type :seq, :child (parse-domain (first x))})

        (set? x)
        ;; x = #{s} -- s is non-terminal
        {:type :seq, :child (parse-domain (first x))}

        (map? x)
        (if (or ((some-fn :strs :keys :syms) x)
                (> (count x) 1))
          {:type :leaf, :bind x}
          (let [[k v] (first x)]
            (if-not (or (coll? v) (symbol? v))
              (throw (Exception. (why-i-disallow-certain-destructuring {k v})))
              {:type :map, :bind k, :child (parse-domain v)})))))

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

(defn modifier-clause
  [{:keys [path leaf type empty empty-leaf-cont]}]
  (fn [result-sym]
    (case type
      :seq `((fnil conj! (transient ~empty)) ~result-sym ~leaf)

      :map `(assoc-in! ~result-sym ~path ~leaf)

      :seq-in-map `(update-in! ~result-sym ~path
                               (fn [x#] ((fnil conj! (transient ~empty-leaf-cont))
                                         x# ~leaf))))))

(defn compile
  [domain range]
  (let [result-sym    (gensym "result")
        structure-sym (gensym "structure")
        parsed-domain (parse-domain domain)
        parsed-range  (parse-range range)
        modifier      ((modifier-clause parsed-range) result-sym)
        default-value (:empty parsed-range)
        nest          (-> parsed-range :nest)]
    (letfn
        [(go [{:keys [type bind child leaf]} ;; parsed domain
              struct-sym]
             (let [next-sym (gensym "val")]
               (case type
                 :map
                 `(doseq [[~bind ~next-sym] ~struct-sym]
                    ~(go child next-sym))

                 :seq
                 `(doseq [~next-sym ~struct-sym]
                    ~(go child next-sym))

                 :leaf
                 `(let [~bind ~struct-sym]
                    ~modifier))))]
      `(fn [~structure-sym]
         (let [~result-sym (transient ~default-value)]
           ~(go parsed-domain structure-sym)
           (deep-persistent! ~nest ~result-sym))))))

(defmacro transformer
  [domain range]
  (compile domain range))

(defmacro transform
  [domain range x]
  `(let [f# (transformer ~domain ~range)]
     (f# ~x)))
