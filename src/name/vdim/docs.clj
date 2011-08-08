;; Docs library is improvement of the find-doc function from clojure.core
;; Features:
;;    - searching separately in name, doc-string and so-on.
;;    - searching in specified namespace.
;;    - searching  specified type of Clojure's statements (macro, function, definition).

(ns name.vdim.docs
  ^{:doc "Docs library is improvement of the find-doc function from clojure.core"}
  [:use clojure.contrib.ns-utils])

(defn find-name
  "Returns list of Clojure's statements (function, macros and so on) by specified its name
  (or part of name or some regex)."
  [n]
  (flatten
    (map
      #(for [v (ns-vars %)
             :when (re-seq (re-pattern n) (str v))] v)
      (all-ns))))

