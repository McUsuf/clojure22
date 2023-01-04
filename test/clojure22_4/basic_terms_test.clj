(ns clojure22-4.basic-terms-test
  (:require
    [clojure.test :refer :all]
    [clojure22-4.basic-terms :refer :all])
  )

(deftest basic-terms-tests
  (testing "primitive terms test"
    (is (variable? (variable :vv)))
    (is (variables-equals? (variable :a) (variable :a)))
    (is (not (variables-equals? (variable :a) (variable :b))))
    (is (const-value (const true))))
  )