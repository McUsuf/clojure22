(ns clojure22-4.utility)


(defn make-expr
  "constructor for expression."
  [type body]
  (cons type body))

(defn expr-type
  "return expression expr type."
  [expr]
  (first expr))

(defn expr-args
  "return arguments of expression expr."
  [expr]
  (rest expr))

(defn expr-type?
  "return true if expr type is expected."
  [expr expected]
  (= (expr-type expr) expected))