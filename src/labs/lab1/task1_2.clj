(ns labs.lab1.task1_2
  (:use labs.lab1.utils))

(defn combine
  ([element list] (combine (str element) list '()))
  ([element list acc] (if (empty? list)
                        acc
                        (recur element (pop list) (if (= (last (str (peek list))) (last element))
                                                    acc
                                                    (conj acc (str element (peek list)))
                                                    )
                               )
                        )
   )
  )

(defn combine-each
  ([elements list] (combine-each elements list '()))
  ([elements list acc] (if (empty? elements)
                         (flatten acc)
                         (recur (drop-last elements) list (conj acc (combine (last elements) list)))
                         )
   ))



(defn perm
  ([list len] (perm list len list))
  ([list len acc] (cond
                    (zero? len) '()
                    (= len 1) acc
                    :else (recur list (dec len) (combine-each acc list))
                    ))
  )

(let [input '(\a \b \c \d)
      size 5
      res (perm input size)]
  (println res)
  (println (assert-not-repeating res))
  (println (count res)))
