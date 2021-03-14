(ns challenge-data-processing.question-04
  (:require
    [clojure.core.match :refer [match]]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.walk :as walk]))


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


(defn find-image-vals
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
       (walk/postwalk find-image-vals)
       keys
       (keep :image-vals)
       (apply set/union)))


(defn file-names
  [image-vals]
  (->> image-vals
       (map #(string/split % #"/"))
       (map last)
       (into #{})))


(defn answer
  [input]
  (->> input
       image-vals
       file-names
       (string/join (System/lineSeparator))))
