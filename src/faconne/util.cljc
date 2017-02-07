(ns faconne.util)

(defn error
  [msg]
  #?(:clj (throw (Exception. msg))
     :cljs (throw (js/Error. msg))))
