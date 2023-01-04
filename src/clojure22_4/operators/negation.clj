(ns clojure22-4.operators.negation
  (:require
    [clojure22-4.utility :refer :all]
    [clojure22-4.basic-terms :refer :all]
    )
  )

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

(defn primitive-or-negation?
  "return true if expression expr is variable or constant or negation"
  [expr]
  (or (primitive? expr) (negation? expr)))
