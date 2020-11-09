(ns clojure2.core)

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
  (if (> dt 0)
    (let [n (/ x dt)] (+ (memo fun dt n) (trapezium-method fun (* dt n) x)))
    0))

;; 2.2
(defn second-opt [fun dt x]
  )

(defn -main []

  (time (println (float (trapezium-method (fn [x] x) 0 21))))
  (time (println (float (first-opt (fn [x] x) 1 21))))
  )
