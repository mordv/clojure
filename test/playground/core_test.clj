(ns playground.core-test
  (:require [clojure.test :refer :all]
            [playground.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(defn quadratic
  [x]
  (* x x))
