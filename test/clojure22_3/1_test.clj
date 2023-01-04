(ns clojure22-3.1-test
  (:require [clojure.test :refer :all]
            [clojure22-3.lab3 :refer [pfilter lazy-pfilter]]))

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

(def test-even-infinite
  {:comment   "Get even nums in infinite range & take 500"
   :predicate even?
   :input     range
   :bound     500
   :expected  (range 0 1000 2)})

(defn test-filter-finite [filter-fn test-case]
  (testing
    (println (str "Test-case: " (test-case :comment) "\n Input=" (test-case :input) "\n Expected=" (test-case :expected)))
    (is (= (filter-fn (test-case :predicate)
                      (test-case :input))
           (test-case :expected)))))

(defn test-filter-infinite [filter-fn test-case]
  (testing
    (println (str "Test-case: " (test-case :comment) "\n Expected=" (test-case :expected)))
    (is (= (->> ((test-case :input))
                (filter-fn (test-case :predicate))
                (take (test-case :bound)))
           (test-case :expected)))))


(defn test-filter [filter-fn test-filter-fn cases]
  (doall (map #(test-filter-fn filter-fn %)
              cases)))

(deftest pfilter-test
  (test-filter pfilter test-filter-finite [test-even test-less-than]))

(deftest lazy-pfilter-test
  (test-filter lazy-pfilter test-filter-infinite [test-even-infinite]))
