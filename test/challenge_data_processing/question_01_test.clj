(ns challenge-data-processing.question-01-test
  (:require
    [challenge-data-processing.csv :as csv]
    [challenge-data-processing.question-01 :as question-01]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest testing is]]
    [jsonista.core :as json]))


(def lipstick (-> "lipstick.json" io/resource slurp json/read-value))


(deftest count-skus-csv-body-test
  (testing "Given a list of items,"
    (let [list-items (-> lipstick
                         (get-in ["mods" "listItems"]))]
      (testing "it should have the same count as original list"
        (is (= 100 (count (question-01/count-skus-csv-body list-items)))))

      (testing "it should count the number of skus"
        (is (= ["'//www.lazada.co.th/products/qianxiu-q127-the-new-moisturizing-and-waterproof-moisturizer-is-not-easy-to-wear-cokkicosmetic-i224295407-s946252295.html?search=1'"
                "\"39.00\""
                "'109.00'"
                6]
               (-> (question-01/count-skus-csv-body list-items) (first)))))

      (testing "it can be formatted as csv"
        (is (= "productUrl,price,originalPrice,numberOfSKUs\n'//www.lazada.co.th/products/qianxiu-q127-the-new-moisturizing-and-waterproof-moisturizer-is-not-easy-to-wear-cokkicosmetic-i224295407-s946252295.html?search=1',\"39.00\",'109.00',6"
               (->> (question-01/count-skus-csv-body list-items)
                    (take 1)
                    (csv/csv ["productUrl" "price" "originalPrice" "numberOfSKUs"]))))))))
