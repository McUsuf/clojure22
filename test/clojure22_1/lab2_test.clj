(ns clojure22-1.lab2_test
  (:require [clojure.test :refer :all]
            [clojure22_2.lab2 :refer [integral-memo
                                      integral-seq area-seq area-lazy-seq
                                      integral-vanilla]]))

(def sin-values
  {:function #(Math/sin %)
   :values   [[0 0]
              [2 1416/1000]
              [3141/1000 2]
              [483/100 883/1000]]})
(def e-values
  {:function #(Math/exp %)
   :values [[1 1718/1000]
            [693/1000 1]
            [404/1000 49/100]
            [0 0]]})

;https://www.wolframalpha.com/input?i2d=true&i=Integrate%5BDivide%5B%5C%2840%292+Power%5Be%2C%5C%2840%292+x%5C%2841%29%5D+sin%5C%2840%29x%5C%2841%29+%5C%2840%29%5C%2840%291+%2B+Power%5Bx%2C2%5D%5C%2841%29+cos%5C%2840%29x%5C%2841%29+%2B+%5C%2840%291+-+x+%2B+Power%5Bx%2C2%5D%5C%2841%29+sin%5C%2840%29x%5C%2841%29%5C%2841%29%5C%2841%29%2CPower%5B%5C%2840%291+%2B+Power%5Bx%2C2%5D%5C%2841%29%2C2%5D%5D%2C%7Bx%2C0%2Ct%7D%5D
(def hard-f-values
  {:function #(/ (* 2
                    (Math/exp (* 2 %))
                    (Math/sin %)
                    (+ (* (+ 1 (Math/pow % 2))
                          (Math/cos %))
                       (* (+ (- 1 %) (Math/pow % 2))
                          (Math/sin %))))
                 (Math/pow (+ 1 (Math/pow % 2)) 2))
   :values [[0 0]
            [2122/1000 919/100]
            [1 2616/1000]
            [3141/1000 0]]})

(defn check-test-fn-results [primary-fn expected-results error]
  (testing
    (doall (map (fn [[input expected]]
                  (do
                    (println (str "Test: input=" input " | expected=" expected))
                    (is (< (Math/abs^double (- expected (primary-fn input)))
                           error))))
                expected-results))))

(defn test-integral [integral-fn step error]
  (doall (map #(check-test-fn-results (integral-fn (% :function) step)
                                      (% :values)
                                      error)
              [sin-values
               e-values
               hard-f-values])))

(deftest integral-memo-test
  (test-integral integral-memo 1/100 1/100))

(deftest integral-seq-default-test
  (test-integral integral-seq 1/100 1/100))

(deftest integral-nolazy-seq-test
  (test-integral (partial integral-seq area-seq) 1/100 1/100))

(deftest integral-lazy-seq-test
  (test-integral (partial integral-seq area-lazy-seq) 1/100 1/100))