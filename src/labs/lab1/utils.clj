(ns labs.lab1.utils)


(defn not-repeated
  ([value] (re-find #"(.)\1+" (str value))))

(defn assert-not-repeating
  ([list] (not-any? not-repeated list)))

