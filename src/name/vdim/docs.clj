;; Docs library is improvement of the find-doc function from clojure.core
;; Features:
;;    - searching separately in name, doc-string and so-on.
;;    - searching in specified namespace.
;;    - searching  specified type of Clojure's statements (macro, function, definition).

(ns name.vdim.docs
  ^{:doc "Docs library is improvement of the find-doc function from clojure.core"}
  [:use clojure.contrib.ns-utils]
  [:import java.util.regex.Pattern])

(defn- f-ns
  "Filters list of all namespaces by specified regex."
  [re]
  (filter #(re-seq (re-pattern re) (-> % .getName name)) (all-ns)))

(defn filter-ns
  "Checks input regex (or string, or vector) and then 
  filters list of all namespaces by f-ns function."
  [re]
  (if re
    (if
      (or (string? re) (= (class re) Pattern)) (f-ns re)
      (flatten (map #(f-ns %) re)))
    (all-ns)))

(defn find-name
  "Returns list of Clojure's statements (function, macros and so on) which are filtered
  by specified its name (or part of name or some regex).
  First param is specified regex for name."
  [n ns]
  (flatten
    (map
      #(map (fn [x] (ns-resolve % x))  
            (filter (fn [x] (re-seq (re-pattern n) (str x))) (ns-vars %)))
      (filter-ns ns))))

(defn ifind-doc
  "Extented version of find-doc function.
  First param is specified regex for name.
  Then different settings is followed.
    :ns <pattern> - define namespace(s) where name is searched.
    :settings (must be set for correct getting) - different settings, which are include:
        :print-doc - define whether doc-string for symbol is printed."
  [re-or-s-or-map & {:keys [ns settings]}]
  (let [lvars (find-name re-or-s-or-map ns)]
    (if (contains? settings :print-doc)
      (doseq [item lvars] (print-doc item))
      lvars)))

