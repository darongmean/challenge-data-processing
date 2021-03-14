(ns challenge-data-processing.question-03
  (:require
    [clojure.walk :as walk]
    [jsonista.core :as json]))


(defn count-products-by-brand-name
  [list-items]
  (let [count-products (fn [[brand-name products]]
                         [brand-name (count products)])]
    (->> list-items
         (group-by #(get % "brandName"))
         (walk/walk count-products identity))))


(defn answer
  [input]
  (->> (get-in input ["mods" "listItems"])
       count-products-by-brand-name
       json/write-value-as-string))
