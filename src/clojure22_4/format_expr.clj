(ns clojure22-4.format-expr
  (:require
    [clojure22-4.basic-terms :refer :all]
    [clojure22-4.utility :refer :all]
    )
  )

(defn build-rules-processor
  [ruleset & args]
  (fn [expr]
    (apply (some (fn [rule]
                   (if ((first rule) expr)
                     (second rule)
                     false))
                 ruleset)
           expr args)))


(declare format-with-vars)

(def ^:private formatting-rules
  (list [variable?
         (fn [var var-val]
           (let [value (var-val var)]
             (if (some? value)
               (const value)
               var)
             )
           )
         ]

        [const? (fn [c] c)]

        [(fn [_] true)
         (fn [expr var-val]
           (let [type (expr-type expr)
                 args (expr-args expr)]
             (make-expr type (doall (map (fn
                                           [ex]
                                           (format-with-vars ex var-val))
                                         args)))
             )
           )
         ]
        ))

(defn format-with-vars
  "format expression expr with vars-dict (:var1 val1 :var2 val2)."
  [expr vars-dict]
  ((build-rules-processor formatting-rules vars-dict) expr))