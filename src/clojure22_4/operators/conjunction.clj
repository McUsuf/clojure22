(ns clojure22-4.operators.conjunction
  (:require [clojure22-4.utility :refer :all]))
(def conj-type ::conj)

(defn conjunction
  "create conjunction of expressions."
  [expr1 & rest]
  (cons conj-type (cons expr1 rest)))

(defn conjunction?
  "return true if expression expr is conjunction."
  [expr]
  (expr-type? expr conj-type))
