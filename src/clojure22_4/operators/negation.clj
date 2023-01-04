(ns clojure22-4.operators.negation
  (:require [clojure22-4.utility :refer :all]))

(defn negation
  "return negation of expression"
  [expr]
  (list ::neg expr))

(defn negation?
  "return true if expr is negate"
  [expr]
  (expr-type? expr ::neg))

(defn not-negation
  "remove negate from expr. expr must be negate"
  [expr]
  {:pre [(negation? expr)]}
  (first (expr-args expr)))
