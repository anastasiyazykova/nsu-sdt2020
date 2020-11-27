(ns clojure3.core
  (:use [clojure.test]))

; 3.1
(defn separation ([block list]
                  (when-let [res (seq list)]
                    (cons (take block res) (separation block (drop block res))))))

(defn my-filter [cond list block]
  (->>
    list
    (separation block)
    (map #(future (doall (filter cond %))))
    (doall)
    (mapcat deref)))

(defn big-boss [sleep pred] (fn [& x]
                              (Thread/sleep sleep)
                              (apply pred x)))

(deftest my-filter-test
  (is (= '(2 4 6) (my-filter (big-boss 100 (fn [x] (= 0 (mod x 2)))) '(1 2 3 4 5 6) 5))))

; 3.2

(defn separation-lazy
  ([block list]
   (lazy-seq
     (when-let [res (seq list)]
       (cons (take block res) (separation-lazy block (drop block res)))))))

(defn do-lazy-task [pred list]
  (->>
    list
    (map #(future (doall (filter pred %))))
    (doall)
    (map deref)
    (lazy-seq)))

(defn my-filter-lazy [pred list block task]
  (->>
    list
    (separation-lazy block)
    (separation-lazy task)
    (map #(do-lazy-task pred %))
    (mapcat identity)
    (mapcat identity)
    )
  )

(deftest my-filter-lazy-test
  (is (= '(0 2 4 6 8) (take 5 (my-filter-lazy (big-boss 100 (fn [x] (= 0 (mod x 2)))) (range) 5 5)))))

(defn -main []
  (time (doall (filter (big-boss 100 (fn [x] (= 0 (mod x 2)))) '(1 2 3 4 5 6 7))))
  (time (my-filter (big-boss 100 (fn [x] (= 0 (mod x 2)))) '(1 2 3 4 5 6 7) 5))

  (time (take 10 (my-filter-lazy (big-boss 100 (fn [x] (= 0 (mod x 2)))) '(1 2 3 4 5 6 7) 5 10)))
  (time (take 10 (my-filter-lazy (big-boss 100 (fn [x] (= 0 (mod x 2)))) (range) 5 5)))

  (my-filter-test)
  (my-filter-lazy-test))