(ns clojure22-1.2)

(defn add-to-word
  ([word alphabet] (add-to-word word alphabet ()))
  ([word alphabet result]
   (if (not-empty alphabet)
     (let [current-element (first alphabet)
           current-res (if (not= current-element (last word))
                         (cons (concat word (list current-element))
                               result)
                         result)]
       (recur word (rest alphabet) current-res))
     result)
   )
  )

(defn update-words
  ([words alphabet] (update-words words alphabet ()))
  ([words alphabet result]
   (if (not-empty words)
     (recur
       (rest words)
       alphabet
       (concat result (add-to-word (first words) alphabet))
       )
     result)
   )
  )

(defn permutations_rec
  ([alphabet n]
   (permutations_rec alphabet n (map #(list %) alphabet))
   )
  ([alphabet n result]
   (if (= 1 n)
     result
     (recur alphabet (- n 1) (update-words result alphabet)))
   )
  )

(defn -main []
  (permutations_rec '("a" [1 2 3] ["e" "g"]) 3))