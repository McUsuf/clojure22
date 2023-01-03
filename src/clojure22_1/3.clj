(ns clojure22-1.3)

(defn my-map [f coll]
  (reduce (fn [acc elem]
            (conj acc (f elem)))
          []
          coll)
  )

(defn my-filter [pred coll]
  (reduce (fn [acc elem]
            (if (pred elem)
              (conj acc elem)
              acc))
          []
          coll))