(ns challenge-data-processing.core-test
  (:require
    [clojure.core.match :refer [match]]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.test :refer [deftest testing is]]
    [clojure.walk :as walk]
    [jsonista.core :as json]))


(defonce lipstick (-> "lipstick.json" io/resource slurp json/read-value))


(defn csv
  [csv-head csv-body]
  (->> (cons csv-head csv-body)
       (map #(string/join "," %))
       (string/join (System/lineSeparator))))


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


(deftest question-05-test
  (let [list-items [{"price" "10" "brandName" "a"}
                    {"price" "5" "brandName" "b"}
                    {"price" "15" "brandName" "b"}
                    {"price" "5" "brandName" "c"}]]
    (is (= [{:balance 5
             :list-items list-items}
            {:balance 0.0
             :list-items [{"price" "5" "brandName" "b"}
                          {"price" "10" "brandName" "a"}
                          {"price" "15" "brandName" "b"}]
             :purchased-items [{"price" "5" "brandName" "c"}]}]
           (->> {:balance 5 :list-items list-items}
                (iterate purchase-min-price)
                (take-while purchase-item-possible?))))
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
                (iterate purchase-min-price)
                (take-while purchase-item-possible?))))
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
                (iterate purchase-min-price)
                (take-while purchase-item-possible?))))))


(deftest question-05-02-test
  (let [list-items (-> lipstick
                       (get-in ["mods" "listItems"]))
        result (->> (purchased-items-maximum-possible 250 list-items)
                    purchased-items-csv-body)]
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
           result))))


(comment
  (def lipstick (-> "lipstick.json" io/resource slurp json/read-value))
  (keys lipstick)
  ; question 01
  (->> (get-in lipstick ["mods" "listItems"])
       count-skus-csv-body
       (csv ["productUrl" "price" "originalPrice" "numberOfSKUs"])
       (spit "target/first.csv"))
  ; question 02
  (->> (get-in lipstick ["mods" "listItems"])
       filtered-price
       avg-price
       (spit "target/second.csv"))
  ; question 03
  (->> (get-in lipstick ["mods" "listItems"])
       count-products-by-brand-name
       json/write-value-as-string
       (spit "target/third.json"))
  ; question 04
  (->> lipstick
       image-vals
       file-names
       (string/join (System/lineSeparator))
       (spit "target/fourth.csv"))
  ; question 05
  (->> (get-in lipstick ["mods" "listItems"])
       (purchased-items-maximum-possible 250)
       purchased-items-csv-body
       (csv ["itemId" "brandName" "price"])
       (spit "target/fifth.csv"))
  (kaocha.repl/run-all))


(kaocha.repl/run-all)
