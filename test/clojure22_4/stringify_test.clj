(ns clojure22-4.stringify-test
  (:require
    [clojure.test :refer :all]
    [clojure22-4.stringify :refer :all]
    [clojure22-4.operators.negation :refer :all]
    [clojure22-4.basic-terms :refer :all]
    [clojure22-4.operators.conjunction :refer :all]
    [clojure22-4.operators.disjunction :refer :all]
    [clojure22-4.format-expr :refer :all]
    [clojure22-4.operators.implication :refer :all]
    )
  )

(deftest stringify-test
  (testing "test for string representation of expression"
    (is (= "!:a"
           (expr-str (negation (variable :a)))))
    (is (= "((:a & !:b & (False || !True)) || :c)"
           (expr-str (disjunction (conjunction (variable :a)
                                               (negation (variable :b))
                                               (disjunction (const false)
                                                            (negation (const true))))
                                  (variable :c)))))))

(deftest stringify-formatted
  (testing "string representation of formatted with variables expression"
    (is (= (expr-str (format-with-vars (implication (disjunction (variable :a)
                                                                 (const true))
                                                    (variable :b))
                                       {(variable :a) false
                                        (variable :b) false}))
           "((False || True) -> False)"))))