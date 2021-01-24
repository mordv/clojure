(ns labs.lab2.task2_test
  (:require [clojure.test :refer :all]
            [playground.core-test :refer :all]
            [labs.lab2.task1 :refer :all]
            [labs.lab2.task2 :refer :all]
            [labs.utils.utils :refer :all]
            ))

(deftest integral-partial-sums-test
  (testing "integral-partial-sums"
    (is (double-eq? 1 ((integral-partial-sums (fn [_] 1)) 1)))
    (is (double-eq? 0.5 ((integral-partial-sums (fn [x] x)) 1)))
    (is (double-eq? 1.99 ((integral-partial-sums (fn [x] (Math/sin x))) 3)))
    (is (double-eq? 9 ((integral-partial-sums (fn [x] (* x x))) 3)))
    (is (not (double-eq? 42 ((integral-partial-sums (fn [x] (* x x))) 3))))
    ))


(deftest time-test
  (testing "testing time optimization"
    (println "load classes:")
    (time ((integral quadratic) 10))
    (println)
    (println "quadratic with integral:")
    (time ((integral quadratic) 1))
    (time ((integral quadratic) 1))
    (time ((integral quadratic) 2))
    (time ((integral quadratic) 2))
    (time ((integral quadratic) 3))
    (time ((integral quadratic) 3))
    (println)
    (println "quadratic with integral-partial-sums:")
    (time ((integral-partial-sums quadratic) 1))
    (time ((integral-partial-sums quadratic) 1))
    (time ((integral-partial-sums quadratic) 2))
    (time ((integral-partial-sums quadratic) 2))
    (time ((integral-partial-sums quadratic) 3))
    (time ((integral-partial-sums quadratic) 3))))
