(ns clojure1.4)

(defn delete-repeat
  [started comb]
  (filter #(not= started (first %)) comb))

(defn add_alphabet_to_word
  [started comb]
  (map #(conj % started) (delete-repeat started comb)))

(defn add_new_combo
  [alphabet comb]
  (mapcat #(add_alphabet_to_word % comb) alphabet))

(defn resolution
  [n alphabet]
  (nth (iterate #(add_new_combo alphabet %) '(())) n))

(defn -main
  [& args]
  (println (resolution (Integer/parseInt (last args)) (drop-last args))))