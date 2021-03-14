(ns challenge-data-processing.csv
  (:require
    [clojure.string :as string]))


(defn csv
  [csv-head csv-body]
  (->> (cons csv-head csv-body)
       (map #(string/join "," %))
       (string/join (System/lineSeparator))))
