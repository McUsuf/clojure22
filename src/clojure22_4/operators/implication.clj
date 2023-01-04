(ns clojure22-4.operators.implication
  (:require [clojure22-4.utility :refer :all]))
(def impl-type ::impl)

(defn implication
  "create implication from expressions. (expr -> rest)"
  [expr & rest]
  (make-expr impl-type (cons expr rest)))

(defn implication?
  "return true if expression expr is implication"
  [expr]
  (expr-type? expr impl-type))
