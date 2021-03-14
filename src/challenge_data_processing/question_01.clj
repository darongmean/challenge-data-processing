(ns challenge-data-processing.question-01
  (:require
    [challenge-data-processing.csv :as csv]))


(defn count-skus-csv-body
  [list-items]
  (for [{:strs [productUrl price originalPrice skus]} list-items]
    [(str "'" productUrl "'")
     (str "\"" price "\"")
     (str "'" originalPrice "'")
     (count skus)]))


(defn answer
  [input]
  (->> (get-in input ["mods" "listItems"])
       count-skus-csv-body
       (csv/csv ["productUrl" "price" "originalPrice" "numberOfSKUs"])))
