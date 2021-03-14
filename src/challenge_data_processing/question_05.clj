(ns challenge-data-processing.question-05
  (:require
    [challenge-data-processing.csv :as csv]))


(defn sort-by-price-asc
  [list-items]
  (into (sorted-set-by (fn [{price-01 "price"} {price-02 "price"}]
                         (<= (Double/parseDouble price-01) (Double/parseDouble price-02))))
        list-items))


(defn purchase-min-price
  [{:keys [balance list-items purchased-items]}]
  (let [[{:strs [brandName price] :as item} & other-items] (sort-by-price-asc list-items)]
    (if (and price (< 0 balance))
      {:balance (- balance (Double/parseDouble price))
       :list-items (remove #(-> % (get "brandName") (= brandName)) other-items)
       :purchased-items (conj purchased-items item)}
      {:balance -1
       :list-items []
       :purchased-items purchased-items})))


(defn purchase-item-possible?
  [{:keys [balance]}]
  (<= 0 balance))


(defn purchased-items-maximum-possible
  [balance list-items]
  (->> {:balance balance :list-items list-items}
       (iterate purchase-min-price)
       (take-while purchase-item-possible?)
       (last)
       (:purchased-items)))


(defn purchased-items-csv-body
  [list-items]
  (for [{:strs [itemId brandName price]} list-items]
    [(str "'" itemId "'")
     (str "'" brandName "'")
     (str "\"" price "\"")]))


(defn answer
  [input]
  (->> (get-in input ["mods" "listItems"])
       (purchased-items-maximum-possible 250)
       purchased-items-csv-body
       (csv/csv ["itemId" "brandName" "price"])))
