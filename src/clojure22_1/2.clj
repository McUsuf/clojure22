(ns clojure22-1.2)

(defn add-to-word [word alphabet]
  (loop [alphabet alphabet result ()]
    (if (not-empty alphabet)
      (let [current-element (first alphabet)
            current-res (if (not= current-element (first word))
                          (cons (concat (list current-element) word)
                                result)
                          result)]
        (recur (rest alphabet) current-res))
      result)))

(defn update-words [words alphabet]
  (loop [words words result ()]
    (if (not-empty words)
      (recur (rest words)
        (concat result (add-to-word (first words) alphabet)))
      result)
    )
  )

(defn perms_iter [n alphabet]
  (nth (iterate #(update-words %1 alphabet)  `(())) n))

(defn -main []
  (perms_iter 3 '("a" [1 2 3] ["e" "g"])))