(ns clojure22-1.4)

(defn add-to-word [word alphabet]
  (map #(conj word %1)
       (filter #(not= %1 (first word))
               alphabet)
       )
  )

(defn update-words [words alphabet]
  (reduce concat
          (map #(add-to-word %1 alphabet)
               words)
          )
  )

(defn permutations-vanilla [n alphabet]
  (nth (iterate #(update-words %1 alphabet)
                '(()))
       n))

(defn -main []
  (permutations-vanilla 3 '("a" [1 2 3] ["e" "g"])))