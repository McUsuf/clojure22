(ns clojure22-4.stringify
  (:require
    [clojure22-4.operators.implication :refer :all]
    [clojure22-4.operators.conjunction :refer :all]
    [clojure22-4.operators.disjunction :refer :all]
    [clojure22-4.operators.negation :refer :all]
    [clojure22-4.basic-terms :refer :all]
    [clojure22-4.utility :refer :all]))

(declare expr-str)

(def delimiters
  "expressions delimiters"
  {conj-type "&"
   disj-type "||"
   impl-type "->"})

(defn ^:private stringify-polyarg
  "construct rule for polyargument expression"
  [args delim]
  (let [head (first args)
        tail (rest args)]
    (clojure.string/join
      (list "("
            (reduce (fn [acc exp] (clojure.string/join (list acc " " delim " " (expr-str exp)))) (expr-str head) tail)
            ")"))))

(def ^:private stringify-rules
  (list [variable?      (fn [v] (str (variable-name v)))]

        [const?         (fn [c] ({false "False", true "True"} (const-value c)))]

        [negation?      (fn [expr] (.concat "!" (expr-str (not-negation expr))))]

        [(fn [_] true)  (fn [expr] (stringify-polyarg
                                     (expr-args expr)
                                     (delimiters (expr-type expr))))]))

(defn expr-str
  [expr]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         stringify-rules)
   expr))
