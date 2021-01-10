(ns clojure4.core)

; Constant

(defn constant [bool]
  {:pre [(boolean? bool)]}
  (list ::const bool))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [c]
  {:pre [(constant? c)]}
  (second c))

; Variable

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-value [v]
  {:pre [(variable? v)]}
  (second v))

; Invert

(defn invert [expr]
  (list ::inv expr))

(defn invert? [expr]
  (= (first expr) ::inv))

(defn invert-value [expr]
  {:pre [(invert? expr)]}
  (second expr)
  )

; Conjunct

(defn conjunct [expr & rest]
  (cons ::conj (cons expr rest)))

(defn conjunct? [expr]
  (= (first expr) ::conj))

; Disjunct

(defn disjunct [expr & rest]
  (cons ::disj (cons expr rest)))

(defn disjunct? [expr]
  (= (first expr) ::disj))

(defn parts [expr]
  {:pre [(or
           (conjunct? expr)
           (disjunct? expr)
           )]}
  (rest expr))

; Implicat

(defn implicat [expr_left expr_right]
  (cons ::impl (list expr_left expr_right)))

(defn implicat? [expr]
  (= (first expr) ::impl))

(defn left [expr]
  {:pre [(implicat? expr)]}
  (second expr))

(defn right [expr]
  {:pre [(implicat? expr)]}
  (nth expr 2))

(defn id [_] _)

(declare dnf)

(def dnf-rules
  (list
    ; Constant
    [constant? id]
    ; Variable
    [variable? id]
    ; Invert
    [invert?
     (fn [expr]
       (let [subexpr (invert-value expr)]
         (cond
           (constant? subexpr) expr
           (variable? subexpr) expr
           (invert? subexpr) (invert-value (invert-value subexpr))
           ; Invert conj/disj
           (disjunct? subexpr) (dnf
                                 (apply conjunct (map
                                                   #(dnf (invert %))
                                                   (parts subexpr))))

           (conjunct? subexpr) (dnf
                                 (apply disjunct (map
                                                   #(dnf (invert %))
                                                   (parts subexpr))))
           ; ¬ (A => B)  =  ¬(¬A or B) = (A and ¬B)
           (implicat? subexpr) (dnf
                                 (conjunct
                                   (left subexpr)
                                   (invert (right subexpr)))))))]
    ; Implicat
    [implicat? (fn [expr]
                 (dnf (disjunct
                        (invert (left expr))
                        (right expr))))]
    ; Disjunct
    [disjunct?
     (fn [expr]
       (apply disjunct (mapcat #(if (disjunct? %)           ; A or (B or C) = A or B or C
                                  (map (fn [arg] (dnf arg)) (parts %))
                                  (list (dnf %)))
                               (parts expr))))]
    [conjunct?
     (fn [expr]
       (let [flattened (mapcat #(if (conjunct? %)
                                  (map (fn [arg] (dnf arg)) (parts %))
                                  (list (dnf %)))
                               (parts expr))
             found_disjunction (first (filter (fn [arg] (disjunct? arg)) flattened))]
         (if (nil? found_disjunction)
           flattened
           ; A and (B or C) = A and B or A and C
           (apply disjunct
                  (map
                    (fn [arg]
                      (apply conjunct (cons arg (filter
                                                  (fn [conjunction] (not (identical? conjunction found_disjunction)))
                                                  flattened
                                                  ))))
                    (parts found_disjunction))))))]))

(defn dnf [expr]
  ((some
     (fn [rule]
       (if ((first rule) expr)
         (second rule)
         false))
     dnf-rules)
   expr))

(defn expr_type [expr] (first expr))

(defn check_eq [expr1 expr2]
  (if (=
        (expr_type expr1)
        (expr_type expr2))

    (cond
      (constant? expr1) (=
                          (constant-value expr1)
                          (constant-value expr2))
      (variable? expr1) (=
                          (variable-value expr1)
                          (variable-value expr2))
      (invert? expr1) (check_eq
                        (invert-value expr1)
                        (invert-value expr2))
      (implicat? expr1) (and
                          (check_eq
                            (left expr1)
                            (left expr2))
                          (check_eq
                            (left expr1)
                            (left expr2)))
      (or
        (disjunct? expr1)
        (conjunct? expr1)) (every?
                             #(check_eq (first %) (second %))
                             (map vector
                                  (parts expr1)
                                  (parts expr2))))
    false))