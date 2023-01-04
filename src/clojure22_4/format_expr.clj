(ns clojure22-4.format-expr
  (:require
    [clojure22-4.basic-terms :refer :all]
    [clojure22-4.utility :refer :all]
    [clojure22-4.operators.negation :refer :all]
    [clojure22-4.operators.conjunction :refer :all]
    [clojure22-4.operators.disjunction :refer :all]
    [clojure22-4.operators.implication :refer :all]
    )
  )

(defn rules-engine
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

        [const? (fn [c _] c)]

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
  "format expression expr with vars-dict {:var1 val1 :var2 val2}."
  [expr vars-dict]
  ((rules-engine formatting-rules vars-dict) expr))

(declare sift-negation)

(def ^:private negation-rules
  (list [primitive?
         identity]

        [negation?
         (fn [expr]
           (let [negated (first (expr-args expr))]
             (cond
               (const? negated) (const (not (const-value negated)))
               (negation? negated) (sift-negation (not-negation negated))
               (disjunction? negated) (apply conjunction
                                             (map (fn [arg]
                                                    (sift-negation (negation arg))) (expr-args negated))
                                             )
               (conjunction? negated) (apply disjunction
                                             (map (fn [arg]
                                                    (sift-negation (negation arg))) (expr-args negated))
                                             )
               true (negation (sift-negation negated))
               )))]

        [(fn [_] true)
         (fn [expr] (let [type (expr-type expr)
                          args (expr-args expr)]
                      (make-expr type (doall (map (fn [ex] (sift-negation ex)) args)))))]
        ))

(defn sift-negation
  "propagate negation down to the variables and constants"
  [expr]
  ((rules-engine negation-rules) expr))

(declare represent-custom-operators)

(def ^:private rules-representation
  (list [primitive?
         identity]

        [implication?
         (fn [expr]
           (let [args (expr-args expr)
                 left (first args)
                 right (second args)]
             (disjunction (negation (represent-custom-operators left))
                          (represent-custom-operators right))))]

        [(fn [_] true)
         (fn [expr] (let [type (expr-type expr)
                          args (expr-args expr)]
                      (make-expr type (doall (map (fn [ex] (represent-custom-operators ex)) args)))))]
        ))

(defn represent-custom-operators
  "represent custom operators in based operators (&,||,!)."
  [expr]
  ((rules-engine rules-representation) expr))


(defn extract-conjunctions
  "extract conjunctions from expr. expr must be disjunction."
  [expr]
  {:pre [(disjunction? expr)]}
  (map (fn [conjunct] (expr-args conjunct)) (expr-args expr)))

(defn disj-of-conjs
  "construct disjunction of conjunctions from args"
  [args]
  (apply disjunction (map
                       (fn [arg-batch]
                         (apply conjunction arg-batch))

                       args)))

(defn cartesian-product
  ([left right]
   (reduce (fn [acc left-arg]
             (concat acc (reduce (fn [acc right-arg]
                                   (conj acc (concat left-arg right-arg)))
                                 () left)))
           () right))
  ([args]
   (reduce (fn [acc arg] (cartesian-product acc arg)) args)))

(declare expand)

(def ^:private expand-rules
  (list [primitive-or-negation? (fn [expr] (disjunction (conjunction expr)))]

        [disjunction? (fn [expr]
                        (let [args (expr-args expr)]
                          (apply disjunction (apply concat (map
                                                             (fn [arg] (expr-args (expand arg)))
                                                             args)))))]

        [conjunction? (fn [expr]
                        (let [args (expr-args expr)]
                          (disj-of-conjs (cartesian-product (map
                                                              (fn [arg] (extract-conjunctions (expand arg)))
                                                              args)))))]
        ))

(defn expand
  "represent expression expr in expanded form. expr must contain only primitives, negatives, disjs or conjs."
  [expr]
  ((rules-engine expand-rules) expr))

(use 'clojure.set)

(defn simplify-conjunction
  "simplify conjunction expression."
  [conjunct]
  (let [literals (filter (fn
                           [literal]
                           (not= literal (const true)))
                         conjunct)]
    (cond
      (empty? literals) (list (const true))

      (some (fn [literal] (= literal (const false))) literals) (list (const false))

      true (let [vars (reduce (fn [acc lit] (if (variable? lit)
                                              (conj acc (variable-name lit))
                                              acc))
                              #{} literals)
                 negated (reduce (fn [acc lit] (if (negation? lit)
                                                 (conj acc (variable-name (not-negation lit)))
                                                 acc))
                                 #{} literals)]
             (if (empty? (intersection vars negated))
               (concat (map (fn [var] (variable var))
                            vars)
                       (map (fn [var] (negation (variable var)))
                            negated))

               (list (const false))))
      )))

(defn simplify-conjunctions
  "simplify conjunctions inside disjunction of conjunctions"
  [expr]
  {:pre [(disjunction? expr)]}
  (apply disjunction (map (fn
                            [conjunct]
                            (apply conjunction
                                   (simplify-conjunction (expr-args conjunct))))
                          (expr-args expr))))

(defn expand-disjunction
  "expand variables from lonely conjunctions"
  [expr]
  {:pre [(disjunction? expr)]}
  (apply disjunction (map (fn
                            [conjunct]
                            (let [args (expr-args conjunct)
                                  args-num (count args)]
                              (if (= args-num 1)
                                (first args)
                                (apply conjunction args))))
                          (expr-args expr)))
  )

; DISJUNCTION SIMPLIFICATION
(defn simplify-disjunction
  "return simplified disjunction expr."
  [expr]
  {:pre [(disjunction? expr)]}
  (let [args (expr-args expr)

        conjuncts (apply list (filter (fn [literal] (not= (const false) literal)) args))

        vars (reduce (fn [acc lit] (if (variable? lit)
                                     (conj acc (variable-name lit))
                                     acc))
                     #{} conjuncts)
        negated (reduce (fn [acc lit] (if (negation? lit)
                                        (conj acc (variable-name (not-negation lit)))
                                        acc))
                        #{} conjuncts)
        other (reduce (fn [acc conjunct]
                        (if (conjunction? conjunct)
                          (conj acc (set (expr-args conjunct)))
                          acc))
                      #{} conjuncts)]
    (cond
      (empty? conjuncts) (const false)

      (some (fn [literal] (= (const true) literal)) conjuncts) (const true)

      (empty? (intersection vars negated)) (apply disjunction (concat (doall (map variable vars))
                                                                      (doall (map (fn [var] (negation (variable var))) negated))
                                                                      (doall (map (fn [args] (apply conjunction args)) other))))
      true (const false)))
  )

(defn to-dnf
  "build dnf of expression expr."
  ([expr]
   (->> expr represent-custom-operators
        sift-negation
        expand
        simplify-conjunctions
        expand-disjunction
        simplify-disjunction))
  ([expr var-vals]
   (to-dnf (format-with-vars expr var-vals))))
