(ns labs.lab3.task1)

(def filter-partition-size 2)

(defn my-partition
  ([part-size coll] (my-partition part-size coll `()))
  ([part-size coll acc] (if (or (not (pos? part-size)) (empty? coll))
                          acc
                          (recur part-size
                                 (drop part-size coll)
                                 (concat acc [(take part-size coll)]))))
  )

(defn my-pmap [f batches]
  (->>
    (map (fn [x] (future (f x))) batches)
    (doall)))

(defn parallel-filter
  [pred coll] (->> (my-partition filter-partition-size coll)
                   (my-pmap (fn [x] (doall (filter pred x))))
                   (map deref)
                   (apply concat)
                   ))
