(ns challenge-data-processing.question-02)


(defn filtered-price
  [list-items]
  (for [{:strs [brandName price skus]} list-items
        :when (= brandName "OEM")
        :when (< 2 (count skus))]
    (try
      (Double/parseDouble price)
      (catch Throwable _ 0))))


(defn avg-price
  [prices]
  {:pre [(pos? (count prices))]}
  (-> (/ (apply + prices) (count prices))
      double))


(defn answer
  [input]
  (->> (get-in input ["mods" "listItems"])
       filtered-price
       avg-price))
