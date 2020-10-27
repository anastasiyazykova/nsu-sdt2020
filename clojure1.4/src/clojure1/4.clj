(ns clojure1.4)

(defn add_alphabet_to_word [started alphabet]
  (filter (fn [it] (not= started (first it))) alphabet)

(defn add_new_combo [started alphabet]
  ()

(defn main_rec [started n alphabet]
  (if (= n 0)
    started
    (main_rec (add_new_combo started alphabet) (- n 1) alphabet)))

(defn resolution [n alphabet]
  (nth (iterate (partial add_new_combo alphabet) '(list)) n))

(defn -main
  [& args]
  (println (resolution (Integer/parseInt (last args)) (drop-last args))))