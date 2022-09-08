(ns clojure22-1.1)

(defn add-chars
  "Возвращает все варианты продолжения строки"
  [s, res, chs]
  (if
    (> (count chs) 0)
    (if
      (= (first s) (first chs))
      (add-chars s res (rest chs))
      (add-chars s
                 (conj res (conj s (first chs)))
                 (rest chs)))
    res))

(defn foo
  [args, res, alphabet]
  (if
    (> (count args) 0)
    (foo
      (rest args)
      (conj res (add-chars (list (first args)) (list) alphabet))
      alphabet)
    res))
(defn task1
  "Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,\n
  состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд."
  [& args]
  (println (foo args (list) args)))