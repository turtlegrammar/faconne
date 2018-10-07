(ns genreadme
  (:require [clojure.string :as string]))

;; Intentionally quadratic
;; This was probably more difficult than Faconne's core logic tbh

(defn gen [lines]
  (let [wrap-src (fn [x] (str "```clj \n" x "\n```"))
        wrap-header (fn [x] (str "##" x))
        wrap-small-header (fn [x] (str "###" x))]
    (loop [[line & rest-lines] lines
           output ""]
      (letfn [(get-src [lines]
                (loop [[line & rest-lines] lines
                       src-chunk ""
                       seen-something? false]
                  (if line
                    (cond (.startsWith line ";;;")
                          [(cons line rest-lines) src-chunk]

                          (empty? line)
                          (recur rest-lines (if seen-something?
                                              (str src-chunk "\n")
                                              src-chunk)
                                 seen-something?)

                          :else
                          (recur rest-lines (str src-chunk "\n" line) true))
                    [nil src-chunk])))]
        (if line
          (cond (.startsWith line ";;;;;")
                (recur rest-lines output)

                (.startsWith line ";;;;2")
                (recur rest-lines (str output "\n" (wrap-small-header (string/join (drop 5 line))) "\n"))

                (.startsWith line ";;;;")
                (recur rest-lines (str output "\n" (wrap-header (string/join (drop 4 line))) "\n"))

                (.startsWith line ";;;")
                (recur rest-lines (str output " " (string/join (drop 3 line))))

                :else
                (let [[rest src] (get-src (cons line rest-lines))]
                  (if (empty? src)
                    (recur rest output)
                    (recur rest (str output "\n```clj" src "\n```\n")))))
          output)))))

(defn gen-from-file
  [f]
  (spit "README.md" (gen (string/split-lines (slurp f)))))
