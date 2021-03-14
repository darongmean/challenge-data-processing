(ns challenge-data-processing.question-02-test
  (:require
    [challenge-data-processing.question-02 :as question-02]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest testing is]]
    [jsonista.core :as json]))


(def lipstick (-> "lipstick.json" io/resource slurp json/read-value))


(deftest question-02-test
  (testing "Given a list of items,"
    (let [list-items (-> lipstick
                         (get-in ["mods" "listItems"]))]
      (testing "filtered-price should return prices of items which has OEM brandName and number of skus more than 2"
        (is (= [39.0 59.0 59.0 59.0 49.0]
               (question-02/filtered-price list-items))))

      (testing "avg-price should compute the average"
        (is (= 53.0
               (-> list-items
                   question-02/filtered-price
                   question-02/avg-price)))))))
