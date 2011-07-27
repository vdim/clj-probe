(ns name.vdim.test-expr
  (:use name.vdim.expr clojure.contrib.test-is))

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

(deftest t-doexpr-double
         ^{:doc "Checks expressions with double number."}
         (is (= (Double/compare (doexpr "10.1+20.4") 30.5)) 0)
         (is (= (Double/compare (doexpr "0.1+5.4") 5.5)) 0)
         (is (= (Double/compare (doexpr "1.00001+0.0") 1.00001)) 0)
         (is (= (Double/compare (doexpr "1.00001+1.0") 2.00001)) 0)
         (is (= (Double/compare (doexpr "1.00001+0") 1.00001)) 0)
         (is (= (Double/compare (doexpr "1.00001+0.1") 1.10001)) 0)
         (is (= (Double/compare (doexpr "0.1*2.4") 0.24)) 0)
         (is (= (Double/compare (doexpr "0.1*0.1") 0.01)) 0)
         (is (= (Double/compare (doexpr "10.1*1") 10.1)) 0)
         (is (= (Double/compare (doexpr "10.1*1.0") 10.1)) 0)
         (is (= (Double/compare (doexpr "10.1*1.1") 11.11)) 0)
         (is (= (Double/compare (doexpr "(10.1+20.4)*0.1") 3.05)) 0))

(deftest div
         ^{:doc "Checks division"}
         (is (= (doexpr "10/2") (/ 10.0 2)))
         (is (= (doexpr "10.0/2") (/ 10.0 2))))

(deftest sub
         ^{:doc "Checks subtraction."}
         (is (= (doexpr "10-2") (- 10.0 2)))
         (is (= (doexpr "10+2-1") 11)))



(time (run-tests))

