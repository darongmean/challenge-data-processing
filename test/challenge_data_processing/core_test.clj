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


(defn reduce-image-vals
  [seq-node]
  (->> seq-node
       (filter map?)
       (apply merge)
       keys
       (filter map?)
       (keep :image-vals)
       flatten
       (apply set/union)))


(defn merge-image-vals
  [map-node]
  (->> map-node
       keys
       (filter map?)
       (apply merge-with set/union)))


(defn extract-image-vals
  [node]
  (let [type-node (when (map-entry? node)
                    :map-entry)]
    (match [type-node node]
      [:map-entry ["image" (image-val :guard string?)]] [{:image-vals #{image-val}} nil]
      [:map-entry [_ ([& seq-node] :seq)]] [{:image-vals (reduce-image-vals seq-node)} nil]
      [:map-entry [_ (map-node :guard map?)]] [(merge-image-vals map-node) nil]
      [:map-entry _] nil
      :else node)))


(defn image-vals
  [lipstick]
  (->> lipstick
       (walk/postwalk extract-image-vals)
       keys
       (keep :image-vals)
       (apply set/union)))


(defn file-names
  [image-vals]
  (->> image-vals
       (map #(string/split % #"/"))
       (map last)
       (into #{})))


(deftest question-04-test
  (let [lipstick {"image" "http://example.org/img01"
                  "other1" "other1"
                  "test1" {"image" [1 2 3]}
                  "test2" {"other2" {"image" "http://example.org/img02"}}
                  "test3" [{"image" "http://example.org/img03"} {"other3" "other3"}]
                  "test4" {"image" [{"image" "http://example.org/img04"} {"other4" "other4"} {"image" "http://example.org/img05"}]}}]
    (is (= #{"http://example.org/img01" "http://example.org/img02" "http://example.org/img03"
             "http://example.org/img04" "http://example.org/img05"}
           (image-vals lipstick)))
    (is (= #{"img01" "img02" "img03" "img04" "img05"}
           (-> lipstick image-vals file-names)))))


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
