(ns labs.lab1.task4
  (:use labs.lab1.task3)
  (:use labs.lab1.utils))


(defn combine-each
  "
    coll.map(n => initColl.filter(a => a != n.first).map(a => a + n))
  "
  [current-collection
   initial-collection]
  (my-map
    (fn [current-element]
      (my-map
        (fn [symbol] (str symbol current-element))
        (my-filter
          (fn [symbol] (not=
                         (str (first (str current-element)))
                         (str symbol)))
          initial-collection)))
    current-collection))

(defn my-flatten
  [coll]
  (reduce (fn [res current]
            (if (sequential? current)
              (concat res current)
              (conj res current))
            )
          []
          coll)
  )

(defn perm
  ([list len] (perm list len list))
  ([list len acc]
   (cond
     (zero? len) '()
     (= len 1) acc
     :else (recur
             list
             (dec len)
             (my-flatten (combine-each acc list))
             ))))

(let [input '(\a \b \c \d)
      size 5
      res (perm input size)]
  (println res)
  (println (assert-not-repeating res))
  (println (count res)))

