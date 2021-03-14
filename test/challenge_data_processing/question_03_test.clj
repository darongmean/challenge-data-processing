(ns challenge-data-processing.question-03-test
  (:require
    [challenge-data-processing.question-03 :as question-03]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest testing is]]
    [jsonista.core :as json]))


(def lipstick (-> "lipstick.json" io/resource slurp json/read-value))


(deftest count-products-by-brand-name-test
  (let [list-items (-> lipstick
                       (get-in ["mods" "listItems"]))
        result (->> list-items
                    question-03/count-products-by-brand-name)]
    (is (= 40
           (count result)))))
