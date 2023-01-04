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

(defn lsplit-seq [coll chunk-size]
  (let [[chunk rest-coll] [(take chunk-size coll) (drop chunk-size coll)]]
    (when (not (empty? coll))
      (lazy-seq (cons chunk
                      (lsplit-seq rest-coll chunk-size))))))

(defn lazy-pfilter [pred coll]
   (->> (lsplit-seq coll
                    (* 64 threads-num))
        (mapcat (partial pfilter pred))))

(defn dummy-predicate [x]
  (Thread/sleep 10)
  (even? x))

(defn test-filter-finite [filter-fn]
  (time (->> (range)
             (take 30)
             (filter-fn dummy-predicate)
             (doall))))


(defn test-filter-infinite [filter-fn]
  (time (->> (range)
             (filter-fn dummy-predicate)
             (take 250)
             (doall))))

(defn -main []
  (let [filters {:vanilla       filter
                 :lazy-parallel lazy-pfilter
                 :parallel      pfilter}]
    (doall (map (fn [[name cur-filter]]
                    (print (str "Filter finite " name ": "))
                    (test-filter-finite cur-filter)
                    (if-not (= name :parallel)
                      (do
                        (print (str "Filter infinite " name ": "))
                        (test-filter-infinite cur-filter)))
                    (println ""))
                  filters))))
