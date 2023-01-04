(ns clojure22-4.format-expr-test
  (:require
    [clojure.test :refer :all]
    [clojure22-4.format-expr :refer :all]
    [clojure22-4.operators.negation :refer :all]
    [clojure22-4.basic-terms :refer :all]
    [clojure22-4.operators.conjunction :refer :all]
    [clojure22-4.operators.disjunction :refer :all]
    [clojure22-4.operators.implication :refer :all]
    )
  )

(deftest format-with-vars-test
  (testing "with variables and vars-dict"
    (is (= (format-with-vars (conjunction (disjunction (variable :a)
                                                       (variable :b))
                                          (negation (variable :c)))
                             {(variable :a) true
                              (variable :b) false
                              (variable :c) true})
           (conjunction (disjunction (const true)
                                     (const false))
                        (negation (const true)))))))

(deftest format-without-vars
  (testing "with variables but vars-dict is empty"
    (is (= (format-with-vars (disjunction (variable :a) (variable :b))
                             {})
           (disjunction (variable :a) (variable :b))))))

(deftest format-partition-vars
  (testing "vars-dict not empty but not full"
    (is (= (format-with-vars (conjunction (disjunction (variable :a)
                                                       (variable :b))
                                          (negation (variable :c)))
                             {(variable :a) true
                              (variable :b) false})

           (conjunction (disjunction (const true)
                                     (const false))
                        (negation (variable :c)))))))

(deftest sift-negation-test
  (testing "sift negation down to const"
    (is (= (sift-negation (negation (negation (negation (negation (const true))))))
           (const true)))
    )
  )

;((¬a→b)∧(¬c∨a))
(def expression (conjunction (implication (negation (variable :a))
                                          (variable :b))
                             (negation (disjunction (variable :c)
                                                    (variable :a)))))

;((¬a→b)∧(¬c∨a)) => (¬a∧¬c∧b)
(deftest to-dnf-test
  (testing "to dnf without vars-dict"
    (is (= (to-dnf expression)

           (disjunction (conjunction (negation (variable :a))
                                     (negation (variable :c))
                                     (variable :b)))))))

(deftest to-dnf-with-vars-dict-test
  (testing "to dnf with vars-dict"
    (is (= (to-dnf expression {(variable :a) true
                               (variable :b) false
                               (variable :c) true})
           (const false)))
    )
  )