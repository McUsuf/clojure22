(ns clojure22-3.1-test
  (:require [clojure.test :refer :all]
            [clojure22-3.lab3 :refer [pfilter]]))

(def test-even
  {:comment "Get even nums in range [0,100)"
   :predicate even?
   :input     (range 100)
   :expected  (range 0 100 2)})

(def test-less-than
  {:comment "Get all nums less than 50 in range [0,100)"
   :predicate #(< % 50)
   :input     (range 100)
   :expected  (range 50)})

(defn test-filter-case [filter-fn test-case]
  (testing
    (println (str "Test-case: " (test-case :comment) "\n Input=" (test-case :input) "\n Expected=" (test-case :expected)))
    (is (= (filter-fn (test-case :predicate)
                      (test-case :input))
           (test-case :expected)))))

(defn test-filter [filter-fn]
  (doall (map #(test-filter-case filter-fn %)
              [test-even
               test-less-than])))

(deftest pfilter-test
  (test-filter pfilter))
