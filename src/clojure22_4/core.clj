(ns clojure22-4.core
  (:require
    [clojure22-4.operators.disjunction :refer :all]
    [clojure22-4.operators.conjunction :refer :all]
    [clojure22-4.operators.negation :refer :all]
    [clojure22-4.operators.implication :refer :all]
    [clojure22-4.basic-terms :refer :all]
    [clojure22-4.utility :refer :all]
    [clojure22-4.stringify :refer :all]
    )
  )

(defn -main []
  (println (expr-str (disjunction (negation (const true))
                                  (conjunction (variable :a) (variable :b)))))

  (println (expr-str (disjunction (negation (implication (variable :a) (variable :c)))
                                  (conjunction (variable :a) (negation (variable :b))))))
  )
