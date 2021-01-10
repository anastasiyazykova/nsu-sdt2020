(ns clojure4.core-test
  (:require [clojure.test :as test]
            [clojure4.core :as dnfp]))

(test/deftest dnf-test
  (let [x (dnfp/variable :x)
        y (dnfp/variable :y)
        z (dnfp/variable :z)]

    (test/testing
      (test/is (dnfp/check_eq (dnfp/dnf (dnfp/conjunct (dnfp/implicat x y) z))
                  (dnfp/disjunct (dnfp/conjunct (dnfp/invert x) z) (dnfp/conjunct y z)))))))

(test/run-tests 'clojure4.core-test)