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


(defn count-skus [list-items]
  (for [{:strs [productUrl price originalPrice skus]} list-items]
    [(str "'" productUrl "'")
     (str "\"" price "\"")
     (str "'" originalPrice "'")
     (count skus)]))


(defn csv [count-skus]
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
           (->> (count-skus list-items) (take 1) csv)))))


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


(defn count-products-by-brand-name [list-items]
  (let [count-products (fn [[brand-name products]]
                         [brand-name (count products)])]
    (->> list-items
         (group-by #(get % "brandName"))
         (walk/walk count-products identity))))


(deftest question-03-test
  (let [list-items (-> lipstick
                       (get-in ["mods" "listItems"]))
        result (->> list-items
                    count-products-by-brand-name)]
    (is (= 40
           (count result)))))


(defn reduce-image-vals [seq-node]
  (->> seq-node
       (filter map?)
       (apply merge)
       keys
       (filter map?)
       (keep :image-vals)
       flatten
       (apply set/union)))


(defn merge-image-vals [map-node]
  (->> map-node
       keys
       (filter map?)
       (apply merge-with set/union)))


(defn extract-image-vals [node]
  (let [type-node (when (map-entry? node)
                    :map-entry)]
    (match [type-node node]
      [:map-entry ["image" (image-val :guard string?)]] [{:image-vals #{image-val}} nil]
      [:map-entry [_ ([& seq-node] :seq)]] [{:image-vals (reduce-image-vals seq-node)} nil]
      [:map-entry [_ (map-node :guard map?)]] [(merge-image-vals map-node) nil]
      [:map-entry _] nil
      :else node)))


(defn image-vals [lipstick]
  (->> lipstick
       (walk/postwalk extract-image-vals)
       keys
       (keep :image-vals)
       (apply set/union)))


(defn file-names [image-vals]
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


(comment
  (def lipstick (-> "lipstick.json" io/resource slurp json/read-value))
  (keys lipstick)
  ; question 01
  (->> (get-in lipstick ["mods" "listItems"])
       count-skus
       csv
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
  (kaocha.repl/run-all))

(kaocha.repl/run-all)
