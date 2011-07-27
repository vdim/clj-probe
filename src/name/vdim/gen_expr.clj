(ns 
  ^{:doc "Library for generating tests for expression grammar."}
  name.vdim.gen-expr
  (:use [clojure.contrib.math :only (round)]))

(def opers
  ^{:doc "List of operations"}
  [+ / - *])

(def opers-ch
  ^{:doc "Character representation of operations"}
  [\+ \/ \- \*])

(defn gen-simple-expr
  "Generates simple expression test: one operation with two operands.
  Returns list where first element is string representation of expression
  and seconde element is result of computing of expression."
  []
  (let [f-arg (rand 1000)
        s-arg (rand 1000)
        op (round (rand 3))]
    [(str f-arg (nth opers-ch op) s-arg) ((nth opers op) f-arg s-arg)]))

(defn gen-complex-expr-type1
  "Generates complex expressions like this:
  (number op number) op (number op number)"
  []
  (let [[f-str f-res] (gen-simple-expr)
        [s-str s-res] (gen-simple-expr)
        op (round (rand 3))]
    [(str \( f-str \) (nth opers-ch op) \( s-str \))
     ((nth opers op) f-res s-res)]))
