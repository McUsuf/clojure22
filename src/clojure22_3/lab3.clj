(ns clojure22-3.lab3)

(def threads-num (.availableProcessors (Runtime/getRuntime)))

(defn split-seq [coll chunk-sizes]
  (let [chunk-size (first chunk-sizes)
        split-coll [(take chunk-size coll) (drop chunk-size coll)]]
    (lazy-seq (cons (first split-coll)
                    (split-seq (second split-coll)
                               (rest chunk-sizes))))))

(defn my-partition [chunks-num coll]
  (let [coll-size (count coll)
        chunk-size (quot coll-size chunks-num)
        rest (rem coll-size chunks-num)]
    (->> (range)
         (map #(if (< % rest)
                 (inc chunk-size)
                 chunk-size))
         (split-seq coll)
         (take chunks-num))))

;(println (my-partition threads-num `(1 2 3 4 5 6 7 8 9 0)))

(defn pfilter [pred coll]
  (->> (my-partition threads-num coll)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))

(defn dummy-predicate [x]
  (Thread/sleep 100)
  (even? x))

(defn test-filter-fn [filter-fn]
  (time (->> (range)
             (take 30)
             (filter-fn dummy-predicate)
             (doall))))

(defn -main []
  (let [filters {:pfilter  pfilter
                 :vanilla  filter}]
    (doall (map (fn [[name cur-filter]]
                    (print (str "Filter " name ": "))
                    (test-filter-fn cur-filter)
                    (println ""))
                  filters))))
