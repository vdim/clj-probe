(ns name.vdim.test-docs
  (:use name.vdim.docs clojure.contrib.test-is))


(deftest t-find-name
         ^{:doc "Tests find-name function."}
         (is (= (find-name "map\\?") '(map?)))
         (is (= (find-name "find-name") '(t-find-name find-name)))
         (is (empty? (find-name "i-think-this-function-is-not-in-any-namespace"))))


