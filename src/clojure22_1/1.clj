(ns clojure22-1.1)

(defn task1-1-helper
  ([alphabet words words_copy result]
   (let [first_letter (first alphabet)
         rest_letters (rest alphabet)
         first_word (first words)
         rest_words (rest words)]
     (cond
       (empty? alphabet) result
       (empty? words) (task1-1-helper rest_letters words_copy words_copy result)
       (= (str (first first_word)) first_letter) (task1-1-helper alphabet rest_words words_copy result)
       true (task1-1-helper alphabet rest_words words_copy (cons (str first_letter first_word) result))
       )
     )
   )
  ([alphabet words] (task1-1-helper alphabet words words ()))
  )

(defn task1-1
  ([alphabet n] (task1-1 (reverse alphabet) n (reverse alphabet)))
  ([alphabet n result]
   (cond
     (= n 1) result
     true (task1-1 alphabet (- n 1) (task1-1-helper alphabet result))
     )
   )
  )

(defn task1
  "Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,\n
  состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд."
  [args, n]
  (println (task1-1 args n))
  )