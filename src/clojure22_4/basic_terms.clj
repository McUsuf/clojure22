(ns clojure22-4.basic-terms
  (:require [clojure22-4.utility :refer :all]))

(defn variable
  "create variable with given name. name must be keyword"
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "return true if expr is variable."
  [expr]
  (expr-type? expr ::var))

(defn variable-name
  "return name of v. v must be variable."
  [v]
  {:pre [(variable? v)]}
  (second v))

(defn variables-equals?
  "return true if v1 equals v2. v1 and v2 must be variables."
  [v1 v2]
  {:pre [(variable? v1) (variable? v2)]}
  (= (variable-name v1)
     (variable-name v2)))

(defn const
  "create constant value. value must be boolean."
  [value]
  {:pre [(boolean? value)]}
  (list ::const value))

(defn const?
  "return true if expr is constant."
  [expr]
  (expr-type? expr ::const))

(defn const-value
  "return value of c. c must be constant."
  [c]
  {:pre [(const? c)]}
  (second c))

(defn primitive?
  "return true if expressions expr is variable or constant."
  [expr]
  (or (variable? expr) (const? expr)))
