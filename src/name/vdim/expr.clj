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
;;    E'-> + T E' | - T E' | ε
;;    T -> F T'
;;    T'-> * F T' | / F E' | ε
;;    F -> (E) | digit
;;    digit -> [0-9]+


(ns name.vdim.expr
  (:use name.choi.joshua.fnparse))

; The parsing state data structure. The remaining tokens are stored
; in :remainder, and the result (result is "stack" with intermediate result of expression)
; is stored in :result.
(defstruct my-state-s :remainder :result :fn)

; Some helper functions.
(defn- do-eval
  "Changes result (res is stack with elements of expression) due to function (f):
  applies f to first and second elements of results and then builds list of
  result of f and rest of res."
  [f res]
  (cons (f (second res) (first res)) (rest (rest res))))

(defn- parse-op
  "Creates parser for some operation (ch as character of operation
  and op as function of operation)."
  [ch p p-prime op]
  (conc 
    (lit ch)
    (complex 
      [ff p
       res (get-info :result)
       si (set-info :result (do-eval op res))]
      ff) 
    p-prime))


; Rules of grammar are below. See BNF in the begining of file.
(def digit
  ^{:doc "Sequence of digits."}
  (lit-alt-seq "0123456789"))

(def whitespaces 
  ^{:doc "Set of whitespaces"}
  (lit-alt-seq " \t\n"))


(def double-number
  ^{:doc "Parser for double numbers"}
  (conc (rep+ digit) (opt (conc (lit \.) (rep+ digit)))))

(declare expr)
(def f (alt (conc (lit \() expr (lit \)))
            (complex [number double-number 
                      res (get-info :result)
                      si (set-info :result (cons (Double/parseDouble (apply str (flatten number))) res))]
                     number)))

(def t-prime (alt 
               (parse-op \* f t-prime *)
               (parse-op \/ f t-prime /)
               emptiness))

(def t (conc f t-prime))

(def e-prime (alt 
               (parse-op \+ t e-prime +)
               (parse-op \- t e-prime -)
               emptiness))

(def expr (conc t e-prime))

; Function for using grammar.
(defn doexpr
  "Computes simple arithmetic expression."
  [e]
  (first (:result ((expr (struct my-state-s (seq e) ())) 1))))

(defn myplus
  [state]
  (let [strn (:remainder state)]
    (if (= (first (:remainder state)) \space)
      (list (:assoc state :result (str "+")) (*remainder-setter* state (rest strn))))))

(defn whitesps
  [state]
  (let [strn (:remainder state) 
        res (:result state)]
    (if (= (first strn) \space)
      (list  (*remainder-setter* state (rest strn))))))


(def gen-tokens
  (rep+ (complex 
          [res (alt myplus whitesps)]
;           r (get-info :result)
;           si (set-info :result (inc r))]
          res)))

(defn do-gen
  "Computes simple arithmetic expression."
  [e]
  (:result ((gen-tokens (struct my-state-s (seq e) "")) 1)))

;(do-gen "+++++")
;(gen-tokens (struct my-state-s (seq "+++++") 0))
;(gen-tokens "+++++")
