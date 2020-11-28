(ns clojure2.core (:use [clojure.test]))

;; 2
(defn trapezium-method [fun a b]
  (* (- b a) (/ (+ (fun a) (fun b)) 2)))

(def memo
  (memoize (fn [fun dt n]
             (if (> n 0)
              (+ (memo fun dt (- n 1)) (trapezium-method fun (* dt (- n 1)) (* dt n)))
             0))))

;; 2.1
(defn first-opt [fun dt x]
  (if (not= dt 0)
    (let [n (/ x dt)] (+ (memo fun dt n) (trapezium-method fun (* dt n) x)))
    0))

;; 2.2
(defn lazy [fun dt]
  (map first (iterate (fn [[acc x]] [(+ (trapezium-method fun x (+ x dt)) acc) (+ x dt)]) [0 0])))

(defn second-opt [fun dt]
    (fn [x] (nth (lazy fun dt) (/ x dt))))

(deftest trapezium-method-test
  (is (= (trapezium-method (fn [x] x) 0 0)) 0)
  (is (= (trapezium-method (fn [x] x) 0 10)) 50)
  (is (= (trapezium-method (fn [x] x) 0 100)) 5000)
  (is (= (trapezium-method (fn [x] x) 3 10)) 91/2)
  (is (= (trapezium-method (fn [x] x) 4 15)) 209/2))

(deftest first-opt-test
  (is (= (first-opt (fn [x] x) 0 0)) 0)
  (is (= (first-opt (fn [x] x) 0 10)) 0)
  (is (= (first-opt (fn [x] x) 1 100)) 5000)
  (is (= (first-opt (fn [x] x) 2 100)) 5000)
  (is (= (first-opt (fn [x] x) 2/10 13)) 169/2))

(deftest second-opt-test
  (is (= ((second-opt (fn [x] x) 1) 100)) 5000)
  (is (= ((second-opt (fn [x] x) 2) 100)) 5000)
  (is (= ((second-opt (fn [x] x) 2/10) 13)) 169/2))

(defn -main []
  (time (float (trapezium-method (fn [x] x) 0 21)))
  (time (float (first-opt (fn [x] x) 1 21)))
  (time (float ((second-opt (fn [x] x) 1) 10)))
  (trapezium-method-test)
  (first-opt-test)
  (second-opt-test))
