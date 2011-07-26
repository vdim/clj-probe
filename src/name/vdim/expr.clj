;; The code parses simple arithmetic expression by fnparse library. 
;; (Fnparse is parser-combinator library for clojure.)
;; BNF for arithmetic expression:
;;    E -> E + T | T
;;    T -> T * F | F
;;    F -> ( E ) | digit
;;    digit -> [0-9]+
;;
;;  BNF without left recursion:
;;    E -> T E'
;;    E'-> + T E'| ε
;;    T -> F T'
;;    T'-> * F T'| ε
;;    F -> (E) | digit
;;    digit -> [0-9]+


(ns name.vdim.expr
  (:use name.choi.joshua.fnparse))

; The parsing state data structure. The remaining tokens are stored
; in :remainder, and the result (result is "stack" with intermediate result of expression)
; is stored in :result.
(defstruct my-state-s :remainder :result)

(def digit (lit-alt-seq "0123456789"))

(defn change-res 
  "Changes result (res is stack with elements of expression) due to function (f):
  applies f to first and second elements of results and then builds list of
  result of f and rest of res."
  [f res]
  (cons (f (first res) (second res)) (rest (rest res))))


(declare expr)
(def f (alt (conc (lit \() expr (lit \)))
            (complex [number (rep+ digit),
                      res (get-info :result)
                      si (set-info :result (cons (Integer/parseInt (apply str number)) res))]
                     number)))

(def t-prime (alt (conc 
                     (lit \*) 
                     (complex 
                       [ff f
                        res (get-info :result)
                        si (set-info :result (change-res * res))]
                       ff) 
                     t-prime) emptiness))

(def t (conc f t-prime))

(def e-prime (alt (conc
                     (lit \+)
                     (complex 
                       [tt t
                        res (get-info :result)
                        si (set-info :result (change-res + res))]
                       tt)
                    
                     e-prime) emptiness))

(def expr (conc t e-prime))

(defn doexpr
  "Computes simple arithmetic expression."
  [e]
  (first (:result ((expr (struct my-state-s (seq e) ())) 1))))
