(ns labs.lab4.lab4_test
  (:require [clojure.test :refer :all]
            [labs.lab4.task1 :refer :all]
            ))

(deftest test-dnf
  (let [x (variable :x)
        y (variable :y)
        z (variable :z)
        t (variable :t)]

    (testing
      "((x | !y) & z) -> (t & !(y -> z)) ==== ((!x & y) | !z) | (t & (y & !z))"
      (is (= (to-dnf
               (implication (conjunction (disjunction x (invert y)) z)
                            (conjunction t (invert (implication y z)))))

             (disjunction (disjunction (conjunction (invert x) y)
                                       (invert z))
                          (conjunction t (conjunction y (invert z))))
             )))))
