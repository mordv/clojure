(ns labs.lab3.task2-test
  (:require [clojure.test :refer :all]
            [labs.lab3.task2 :refer :all]))

(defn heavy-predicate [x]
  (Thread/sleep 100)
  (even? x))

(def list1 `(1 2 3 4 5 6))
(def list2 `(8 7 6 42 228 1337))
(def empty-list `())


(def naturals (iterate inc 1))

(deftest correctness-test-lazy
  (testing "testing lazy-parallel-filter correctness"
    (is (= (filter even? list1) (lazy-parallel-filter even? list1)))
    (is (= (filter even? list2) (lazy-parallel-filter even? list2)))
    (is (= (filter even? empty-list) (lazy-parallel-filter even? empty-list)))
    (is (= (filter heavy-predicate list1) (lazy-parallel-filter heavy-predicate list1)))
    (is (= (filter heavy-predicate list2) (lazy-parallel-filter heavy-predicate list2)))
    (is (= (filter heavy-predicate empty-list) (lazy-parallel-filter heavy-predicate empty-list)))
    (is (= (take 10 (filter even? naturals)) (take 10 (lazy-parallel-filter even? naturals))))
    ))

(deftest time-test-with-regular-pred-lazy
  (testing "testing filter vs lazy-parallel-filter with regular predicate"
    (println "load classes:")
    (time (println (lazy-parallel-filter even? (range 0 100))))
    (println)
    (println "with filter:")
    (time (println (filter even? list1)))
    (time (println (filter even? list2)))
    (time (println (take 10 (filter even? naturals))))
    (println)
    (println "with pfilter:")
    (time (println (lazy-parallel-filter even? list1)))
    (time (println (lazy-parallel-filter even? list2)))
    (time (println (take 10 (lazy-parallel-filter even? naturals))))
    ))

(deftest time-test-with-heavy-predicate-lazy
  (testing "testing filter vs lazy-parallel-filter with long running predicate"
    (let [list1 (list 1 2 3 4 5 6 7 8)
          list2 (list 3 7 1 5 8 4 2 6)]
      (println "load classes:")
      (time (println (lazy-parallel-filter heavy-predicate (range 0 10))))
      (println)
      (println "with filter:")
      (time (println (filter heavy-predicate list1)))
      (time (println (filter heavy-predicate list2)))
      (time (println (filter heavy-predicate (range 100))))
      (time (println (take 10 (filter heavy-predicate naturals))))
      (println)
      (println "with pfilter:")
      (time (println (lazy-parallel-filter heavy-predicate list1)))
      (time (println (lazy-parallel-filter heavy-predicate list2)))
      (time (println (lazy-parallel-filter heavy-predicate (range 100))))
      (time (println (take 10 (lazy-parallel-filter heavy-predicate naturals)))))))

(deftest correctness-test
  (testing "testing lazy-parallel-filter correctness"
    (is (= (filter even? list1) (lazy-parallel-filter even? list1)))
    (is (= (filter even? list2) (lazy-parallel-filter even? list2)))
    (is (= (filter even? empty-list) (lazy-parallel-filter even? empty-list)))
    (is (= (filter heavy-predicate list1) (lazy-parallel-filter heavy-predicate list1)))
    (is (= (filter heavy-predicate list2) (lazy-parallel-filter heavy-predicate list2)))
    (is (= (filter heavy-predicate empty-list) (lazy-parallel-filter heavy-predicate empty-list)))
    ))
