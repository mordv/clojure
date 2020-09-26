(ns main)

(defn myFunc
  ([] (println "no args"))
  ([x] (println x)))

(defn types
  ([] (let [list '(1 2 3)
            vector [1 2 3]
            set #{1 2 3}                                    ;; exception for duplications!!!
            map #{:a 1 :b 2}                                ;; exception for duplications!!!
            string "string"
            num 528
            ratio 2/4]
        (println list vector set map string num ratio)
        (println (type list) (type vector) (type set) (type map) (type string) (type num) (type ratio))) ;; The fuck is Persistent prefix on typename?
   ))


;; LISTS
(defn lists
  ([] (let [first `(1 2 3)
            second `(4 5 6)
            third `(3 4)
            stringList `("one" "two" "three")]
        (println (count first))
        (println (seq first))
        (println (get first 0))
        )))

;; VECTORS
(defn vectors
  ([] (let [first [1 2 3]]
        (println (get first 1))
        )))

(defn maps
  ([] (let [first {:a 1 :b 2}]
        (println first)
        (println "a: " (:a first))
        (println "b: " (:b first))
        )))

(defn ifSide
  ([] (print (if true
               (do (println "true")                         ;; side effect (WHY???)
                   "something")
               ))
   ))

(defn ifKeyword
  ([] (println (if :anyKeywordIsTrue "true" "false"))
   ))

(defn condTest
  [x] (cond
        (pos? x) "positive"
        (zero? x) "zero"
        :else "negative")
  )

(defn caseTest
  [x] (case x
        5 "x is 5"
        "Optional trailing expr"
        ))

(defn factorial
  ([n] (factorial n 1))
  ([n f] (if (< n 1)
           f
           (recur (dec n) (* n f)))
   ))

;(types)
;(lists)
;(vectors)
;(maps)
;(ifSide)
;(println (caseTest 5))
;(println (factorial 5))

