(ns clojure22-3.lab3)

(def threads-num (.availableProcessors (Runtime/getRuntime)))

(defn split-seq [coll chunk-sizes]
  (let [chunk-size (first chunk-sizes)
        split-coll (split-at chunk-size coll)]
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

(println (my-partition threads-num `(1 2 3 4 5 6 7 8 9 0)))

(defn pfilter [pred coll]
  (->> (my-partition threads-num coll)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))


