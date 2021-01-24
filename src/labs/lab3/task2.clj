(ns labs.lab3.task2
  (:require [labs.lab3.task1 :refer :all]))

(def def-filter-partition-size 10)
(def def-thread-count 10)

(defn lazy-parallel-filter
  ([pred coll] (lazy-parallel-filter pred def-filter-partition-size def-thread-count coll))
  ([pred sublist-size thread-count coll]
   (->> (partition-all (* sublist-size thread-count) coll)
        (map (fn [list] (->> (partition-all sublist-size list)
                             (map (fn [x] (future (doall (filter pred x)))))
                             (doall)
                             (map deref))))
        (flatten))))
