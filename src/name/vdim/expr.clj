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


(ns name.vdim
  (:use name.choi.joshua.fnparse 
        clojure.contrib.test-is))

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

(def expr (conc t e-second))

(defn doexpr
  "Computes simple arithmetic expression."
  [e]
  (first (:result ((expr (struct my-state-s (seq e) ())) 1))))


(deftest t-doexpr
         ^{:doc "Checks simple expression"}
         (is (= (doexpr "2") 2))
         (is (= (doexpr "1+0") 1))
         (is (= (doexpr "1+1") 2))
         (is (= (doexpr "2+3") 5))
         (is (= (doexpr "1*0") 0))
         (is (= (doexpr "1*1") 1))
         (is (= (doexpr "2*3") 6))
         (is (= (doexpr "1+3*2") 7))
         (is (= (doexpr "(1+3)*2") 8))
         (is (= (doexpr "(1*5)*4") 20))
         (is (= (doexpr "(0+1)*(3+2)*(0+1)*(0+1)") 5)))


(deftest t-doexpr-number
         ^{:doc "Checks expression with number containing more than one digis."}
         (is (= (doexpr "10+20") 30))
         (is (= (doexpr "10*20") 200))
         (is (= (doexpr "90+110") 200))
         (is (= (doexpr "90+1") 91))
         (is (= (doexpr "100000+20") 100020))
         (is (= (doexpr "(10+1)*(90+10)") 1100)))

(time (run-tests))

