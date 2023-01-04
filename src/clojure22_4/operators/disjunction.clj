(ns clojure22-4.operators.disjunction
  (:require [clojure22-4.utility :refer :all]))

(def disj-type ::disj)

(defn disjunction
  "create disjunction of expressions"
  [expr & rest]
  (cons disj-type (cons expr rest)))

(defn disjunction?
  "return true if expression expr is disjunction"
  [expr]
  (expr-type? expr disj-type))