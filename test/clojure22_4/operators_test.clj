(ns clojure22-4.operators-test
  (:require
    [clojure.test :refer :all]
    [clojure22-4.operators.negation :refer :all]
    [clojure22-4.operators.conjunction :refer :all]
    [clojure22-4.operators.disjunction :refer :all]
    [clojure22-4.operators.implication :refer :all]
    [clojure22-4.basic-terms :refer :all])
  )

(deftest operators-tests
  (testing
    (is (negation? (negation (const true))))
    (is (not (disjunction? (conjunction (variable :sample1) (variable :sample2)))))
    (is (not (conjunction? (disjunction (variable :sample)))))
    (is (implication? (implication (variable :sample) (const true))))
    )
  )
