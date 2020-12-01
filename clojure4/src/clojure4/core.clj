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
  (second expr))

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

(def dnf-rules
  (
    
    ))

(defn dnf [expr]
  ((some
     (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
     dnf-rules)
   expr))