(ns clojure22_2.lab2)

(def step 1/100)

;((x + 1)^2) / (e^x * cos(2x))
(defn test-fn [x]
  (double
    (/ (* (Math/exp x)
          (Math/cos (* 2 x)))
       (Math/pow (+ x 1) 2))))

(defmacro stopwatch [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     ret#
     (double (/ (- (. System (nanoTime))
                   start#)
                1000000.0))))

(defn trapezoid-area [f x_i step]
  (* (/ (+ (f x_i)
           (f (+ x_i step)))
        2)
     step))

(defn integral [f area-fn step]
  #(let [steps (/ % step)]
     (reduce (fn [acc i]
               (+ acc
                  (area-fn f (* step i) step)
                  )
               )
             0
             (range steps))))

(defn integral-vanilla [f step]
  (integral f trapezoid-area step))

(def area-memo
  (memoize (fn [rec-fn f x_i step]
             (if (not= 0 x_i)
               (+ (rec-fn rec-fn f (- x_i step) step)
                  (* (/ (+ (f x_i)
                           (f (- x_i step)))
                        2)
                     step))
               0.0))))

(defn integral-memo [f step]
  (let [area-memos (partial area-memo area-memo)]
    #(area-memos f
                 (* step (quot % step))
                 step))
  )

(defn area-seq [f step]
  (map first
       (iterate (fn [[areas x_i]]
                  [(+ areas (trapezoid-area f x_i step)) (+ x_i step)])
                [0 0])))

(defn area-lazy-seq
  ([f step]
   (area-lazy-seq f step 0 0.0))
  ([f step i areas]
   (lazy-seq
     (cons areas
           (area-lazy-seq f
                          step
                          (inc i)
                          (+ (trapezoid-area f (* i step) step)
                             areas))))))

(defn integral-seq
  ([f step]
   (integral-seq area-seq f step))
  ([area-f f step]
   (let [seq (area-f f step)]
     (fn [x] (nth seq (/ x step))))))

(defn -main []
  (let [primitives {:vanilla   (integral-vanilla test-fn step)
                    :memoized  (integral-memo test-fn step)
                    :sequenced (integral-seq test-fn step)}]
    (doall (for [delta (take 20 (range))]
             (map (fn [[name primitive]]
                    (println (str delta " " name ": " (stopwatch (primitive (+ 1 (/ delta 10)))) " msecs")))
                  primitives)))))