; Code for testing whcount library.
; @author Vyacheslav Dimitrov <vycheslav.dimitrov@gmail.com>
; Format of file with developer's working hours:
; # If line is started with "#" symbol, then line is comment.
; # Also empty line is allowed, such as next.
;
; # <date> <activity> <count of hours> <description>
; 06.08.2011  CO  1   Coding get-stat function for whcount library
; 06.08.2011  TE  0.5   Testing get-stat function 
; 06.08.2011  ME  1   Meeting with team leader.
; # and so on.
; # See example of file in wh-vdim.txt

(ns name.vdim.test-whcount
  ^{:doc "Code for testing whcount library."}
  (:use name.vdim.whcount clojure.contrib.test-is))

(deftest t-get-hours
         ^{:doc "Checks simple expression"}
         (is (= (get-hours "2.0") 2))
         (is (= (get-hours "2") 2))
         (is (= (get-hours "200.5") 200.5))
         (is (= (get-hours "fail format") 0)))

