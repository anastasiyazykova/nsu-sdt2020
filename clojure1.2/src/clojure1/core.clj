(ns clojure1.core)

(defn add_alphabet_to_word [started alphabet]
  (loop [comb '() started started alphabet alphabet]
  (if (empty? alphabet)
    comb
    (let [firs (first alphabet) first_in_started (first started)]
      (if (not= firs first_in_started)
        (recur (cons (cons firs started) comb) started (rest alphabet))
        (recur comb started (rest alphabet)))))))

(defn add_new_combo [started alphabet]
  (loop [tail '() started started]
    (if (empty? started)
      tail
      (recur (concat tail (add_alphabet_to_word (first started) alphabet)) (rest started)))))

(defn main_rec [started n alphabet]
  (if (= n 0)
    started
    (recur (add_new_combo started alphabet) (- n 1) alphabet)))

(defn resolution [n alphabet]
  (let [started (main_rec '(()) 1 alphabet)]
    (if (= n 0)
      started
      (main_rec started (- n 1) alphabet))))

(defn -main
  [& args]
  (println (resolution (Integer/parseInt (last args)) (drop-last args))))