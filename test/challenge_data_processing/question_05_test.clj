(ns challenge-data-processing.question-05-test
  (:require
    [challenge-data-processing.question-05 :as question-05]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest testing is]]
    [jsonista.core :as json]))


(defonce lipstick (-> "lipstick.json" io/resource slurp json/read-value))


(deftest purchase-min-price-test
  (testing "Given a list of items,"
    (let [list-items [{"price" "10" "brandName" "a"}
                      {"price" "5" "brandName" "b"}
                      {"price" "15" "brandName" "b"}
                      {"price" "5" "brandName" "c"}]]
      (testing "purchase-min-price should pick the smallest price"
        (is (= [{:balance 5
                 :list-items list-items}
                {:balance 0.0
                 :list-items [{"price" "5" "brandName" "b"}
                              {"price" "10" "brandName" "a"}
                              {"price" "15" "brandName" "b"}]
                 :purchased-items [{"price" "5" "brandName" "c"}]}]
               (->> {:balance 5 :list-items list-items}
                    (iterate question-05/purchase-min-price)
                    (take-while question-05/purchase-item-possible?)))))

      (testing "When there isn't enough balance, purchase-item-possible? should terminate"
        (is (= [{:balance 10
                 :list-items list-items}
                {:balance 5.0
                 :list-items [{"price" "5" "brandName" "b"}
                              {"price" "10" "brandName" "a"}
                              {"price" "15" "brandName" "b"}]
                 :purchased-items [{"price" "5" "brandName" "c"}]}
                {:balance 0.0
                 :list-items [{"price" "10" "brandName" "a"}]
                 :purchased-items [{"price" "5" "brandName" "b"} {"price" "5" "brandName" "c"}]}]
               (->> {:balance 10 :list-items list-items}
                    (iterate question-05/purchase-min-price)
                    (take-while question-05/purchase-item-possible?)))))

      (testing "When there isn't enough items to pick, purchase-item-possible? should terminate"
        (is (= [{:balance 50
                 :list-items list-items}
                {:balance 45.0
                 :list-items [{"price" "5" "brandName" "b"}
                              {"price" "10" "brandName" "a"}
                              {"price" "15" "brandName" "b"}]
                 :purchased-items [{"price" "5" "brandName" "c"}]}
                {:balance 40.0
                 :list-items [{"price" "10" "brandName" "a"}]
                 :purchased-items [{"price" "5" "brandName" "b"} {"price" "5" "brandName" "c"}]}
                {:balance 30.0
                 :list-items []
                 :purchased-items [{"price" "10" "brandName" "a"} {"price" "5" "brandName" "b"} {"price" "5" "brandName" "c"}]}]
               (->> {:balance 50 :list-items list-items}
                    (iterate question-05/purchase-min-price)
                    (take-while question-05/purchase-item-possible?))))))))


(deftest purchased-items-csv-body-test
  (let [list-items (-> lipstick
                       (get-in ["mods" "listItems"]))]
    (is (= [["'224295407'" "'OEM'" "\"39.00\""]
            ["'341864493'" "'Novo'" "\"39.00\""]
            ["'331292065'" "'IMAGES'" "\"29.00\""]
            ["'350478554'" "'ZIRANMI'" "\"29.00\""]
            ["'346252619'" "'PEINIFEN'" "\"25.00\""]
            ["'278125439'" "'sivanna'" "\"24.00\""]
            ["'523686357'" "'Kiss Beauty'" "\"20.00\""]
            ["'316046847'" "'MENOW'" "\"19.00\""]
            ["'543952820'" "'Tanako'" "\"15.00\""]
            ["'502436001'" "'No Brand'" "\"7.00\""]]
           (->> (question-05/purchased-items-maximum-possible 250 list-items)
                question-05/purchased-items-csv-body)))))
