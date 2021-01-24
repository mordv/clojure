(ns labs.lab3.task1-test
  (:require [clojure.test :refer :all]
            [labs.lab3.task1 :refer :all]))

(deftest my-partition-test
  (testing "my-partition-test"
    (is (= [[1] [2] [3]] (my-partition 1 [1 2 3])))
    (is (= [[1 2] [3]] (my-partition 2 [1 2 3])))
    (is (= [[1 2 3]] (my-partition 3 [1 2 3])))
    (is (= [[1 2 3]] (my-partition 4 [1 2 3])))
    ))

(defn heavy-predicate [x]
  (Thread/sleep 100)
  (even? x))

(def list1 `(1 2 3 4 5 6))
(def list2 `(8 7 6 42 228 1337))
(def empty-list `())

(deftest correctness-test
  (testing "testing parallel-filter correctness"
    (is (= (filter even? list1) (parallel-filter even? list1)))
    (is (= (filter even? list2) (parallel-filter even? list2)))
    (is (= (filter even? empty-list) (parallel-filter even? empty-list)))
    (is (= (filter heavy-predicate list1) (parallel-filter heavy-predicate list1)))
    (is (= (filter heavy-predicate list2) (parallel-filter heavy-predicate list2)))
    (is (= (filter heavy-predicate empty-list) (parallel-filter heavy-predicate empty-list)))
    ))

(deftest time-test-with-regular-pred
  (testing "testing filter vs parallel-filter with regular predicate"
    (println "load classes:")
    (time (println (parallel-filter even? (range 0 100))))
    (println)
    (println "with filter:")
    (time (println (filter even? list1)))
    (time (println (filter even? list2)))
    (println)
    (println "with parallel-filter:")
    (time (println (parallel-filter even? list1)))
    (time (println (parallel-filter even? list2)))
    ))

(deftest time-test-with-long-running-pred
  (testing "testing filter vs parallel-filter with long running predicate"
    (println "load classes:")
    (time (println (parallel-filter heavy-predicate (range 0 10))))
    (println)
    (println "with filter:")
    (time (println (filter heavy-predicate list1)))
    (time (println (filter heavy-predicate list2)))
    (time (println (filter heavy-predicate (range 100))))
    (println)
    (println "with parallel-filter:")
    (time (println (parallel-filter heavy-predicate list1)))
    (time (println (parallel-filter heavy-predicate list2)))
    (time (println (parallel-filter heavy-predicate (range 100))))
    ))
