(ns labs.lab1.task3)


(defn my-map
  [fun coll]
  (reduce (fn [a b] (conj a (fun b))) [] coll))

(defn my-filter
  [fun coll]
  (reduce
    (fn [a b]
      (if (fun b)
        (conj a b)
        a))
    [] coll))

(println (my-map (fn [a] (* a 2)) [1 2 3 4 5]))
(println (my-filter (fn [a] (<= a 2)) [1 2 3 4 5]))
