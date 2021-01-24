(ns labs.lab3.task1)

(def filter-partition-size 10)

(defn my-partition
  ([part-size coll] (my-partition part-size coll `()))
  ([part-size coll acc] (if (or (not (pos? part-size)) (empty? coll))
                          acc
                          (recur part-size
                                 (drop part-size coll)
                                 (concat acc [(take part-size coll)]))))
  )

(defn parallel-filter
  [pred coll] (->> (my-partition filter-partition-size coll)
                   (map (fn [part-coll] (future (doall (filter pred part-coll)))))
                   (doall)
                   (map deref)
                   (flatten)
                   ))
