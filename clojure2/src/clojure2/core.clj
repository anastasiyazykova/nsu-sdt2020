(ns clojure2.core)

(defn resolution [fun a b]
  (* (- b a) (/ (+ (fun a) (fun b)) 2.0)))

(def memo
  (memoize (fn [fun dt n]
             (if (> n 0.0)
              (+ (memo fun dt (- n 1)) (resolution fun (* dt (- n 1)) (* dt n)))
             0.0))))

(defn first-opt [fun dt x]
  (if (> dt 0.0)
    (let [n (/ x dt)] (+ (memo fun dt n) (resolution fun (* dt n) x)))
    0.0))

(defn -main []

  (time (println (resolution (fn [x] x) 0.0 20.0)))

  (time (println (first-opt (fn [x] x) 1.0 20.0))))
