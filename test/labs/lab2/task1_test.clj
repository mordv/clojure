(ns labs.lab2.task1_test
  (:require [clojure.test :refer :all]
            [playground.core-test :refer :all]
            [labs.lab2.task1 :refer :all]
            [labs.utils.utils :refer :all]
            ))


(deftest trapezoid-area-test
  (testing "trapezoid-area"
    (is (= 2.5 (trapezoid-area 2 3 1)))
    (is (= 1. (trapezoid-area 1 1 1)))
    (is (= 132. (trapezoid-area 10 2 22)))
    (is (not= 0.088 (trapezoid-area 0.9 0.85 0.1)))
    ))


(deftest integral-function-test
  (testing "integral-function"
    (is (double-eq? 1 (integral-function (fn [_] 1) 1)))
    (is (double-eq? 0.5 (integral-function (fn [x] x) 1)))
    (is (double-eq? 1.99 (integral-function (fn [x] (Math/sin x)) 3)))
    (is (double-eq? 9 (integral-function (fn [x] (* x x)) 3)))
    (is (not (double-eq? 42 (integral-function (fn [x] (* x x)) 3))))
    ))

(deftest memoized-integral-function-test
  (testing "memoized-integral-function"
    (is (double-eq? 1 (memoized-integral-function (fn [_] 1) 1)))
    (is (double-eq? 0.5 (memoized-integral-function (fn [x] x) 1)))
    (is (double-eq? 1.99 (memoized-integral-function (fn [x] (Math/sin x)) 3)))
    (is (double-eq? 9 (memoized-integral-function (fn [x] (* x x)) 3)))
    (is (not (double-eq? 42 (memoized-integral-function (fn [x] (* x x)) 3))))
    ))

(deftest integral-test
  (testing "integral"
    (is (double-eq? 1 ((integral (fn [_] 1)) 1)))
    (is (double-eq? 0.5 ((integral (fn [x] x)) 1)))
    (is (double-eq? 1.99 ((integral (fn [x] (Math/sin x))) 3)))
    (is (double-eq? 9 ((integral (fn [x] (* x x))) 3)))
    (is (not (double-eq? 42 ((integral (fn [x] (* x x))) 3))))
    ))

(deftest memoized-integral-test
  (testing "memoized-integral"
    (is (double-eq? 1 ((memoized-integral (fn [_] 1)) 1)))
    (is (double-eq? 0.5 ((memoized-integral (fn [x] x)) 1)))
    (is (double-eq? 1.99 ((memoized-integral (fn [x] (Math/sin x))) 3)))
    (is (double-eq? 9 ((memoized-integral (fn [x] (* x x))) 3)))
    (is (not (double-eq? 42 ((memoized-integral (fn [x] (* x x))) 3))))
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
    (println "quadratic with memoized integral:")
    (time ((memoized-integral quadratic) 1))
    (time ((memoized-integral quadratic) 1))
    (time ((memoized-integral quadratic) 2))
    (time ((memoized-integral quadratic) 2))
    (time ((memoized-integral quadratic) 3))
    (time ((memoized-integral quadratic) 3))))
