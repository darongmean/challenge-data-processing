(ns challenge-data-processing.core-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure.string :as string]
    [clojure.java.io :as io]
    [jsonista.core :as json]))


(defonce lipstick (-> "lipstick.json" io/resource slurp json/read-value))


(defn count-skus [list-items]
  (for [{:strs [productUrl price originalPrice skus]} list-items]
    [(str "'" productUrl "'")
     (str "\"" price "\"")
     (str "'" originalPrice "'")
     (count skus)]))


(defn as-csv [count-skus]
  (->> (cons ["productUrl" "price" "originalPrice" "numberOfSKUs"] count-skus)
       (map #(string/join "," %))
       (string/join (System/lineSeparator))))


(deftest question-01-test
  (let [list-items (-> lipstick
                       (get-in ["mods" "listItems"]))]
    (is (= 100 (count (count-skus list-items))))
    (is (= ["'//www.lazada.co.th/products/qianxiu-q127-the-new-moisturizing-and-waterproof-moisturizer-is-not-easy-to-wear-cokkicosmetic-i224295407-s946252295.html?search=1'"
            "\"39.00\""
            "'109.00'"
            6]
           (-> (count-skus list-items) (first))))
    (is (= "productUrl,price,originalPrice,numberOfSKUs\n'//www.lazada.co.th/products/qianxiu-q127-the-new-moisturizing-and-waterproof-moisturizer-is-not-easy-to-wear-cokkicosmetic-i224295407-s946252295.html?search=1',\"39.00\",'109.00',6"
           (->> (count-skus list-items) (take 1) as-csv)))))


(defn filtered-price [list-items]
  (for [{:strs [brandName price skus]} list-items
        :when (= brandName "OEM")
        :when (< 2 (count skus))]
    (try
      (Double/parseDouble price)
      (catch Throwable _ 0))))


(defn avg-price [prices]
  {:pre [(pos? (count prices))]}
  (-> (/ (apply + prices) (count prices))
      double))


(deftest question-02-test
  (let [list-items (-> lipstick
                       (get-in ["mods" "listItems"]))]
    (is (= [39.0 59.0 59.0 59.0 49.0]
           (filtered-price list-items)))
    (is (= 53.0
           (-> list-items filtered-price avg-price)))))


(comment
  (def lipstick (-> "lipstick.json" io/resource slurp json/read-value))
  (keys lipstick)
  ; question 01
  (->> (get-in lipstick ["mods" "listItems"])
       count-skus
       as-csv
       (spit "target/first.csv"))
  ; question 02
  (->> (get-in lipstick ["mods" "listItems"])
       filtered-price
       avg-price
       (spit "target/second.csv"))
  (kaocha.repl/run-all))

(kaocha.repl/run-all)
