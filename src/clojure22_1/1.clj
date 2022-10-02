(ns clojure22-1.1)

(defn add-to-word [word alphabet]
  (when (not-empty alphabet)
    (let [result (add-to-word word (rest alphabet))]
      (if (not= (first alphabet) (first word))
        (conj result (conj word (first alphabet)))
        result)
      )
    )
  )

(defn update-words [words alphabet]
  (when (not-empty words)
    (concat (update-words (rest words) alphabet)
            (add-to-word (first words) alphabet))
    )
  )

(defn permutations [alphabet n]
  (if (> n 0)
    (update-words (permutations alphabet (- n 1)) alphabet)
    '(())
    )
  )