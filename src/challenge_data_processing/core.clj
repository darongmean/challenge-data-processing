(ns challenge-data-processing.core
  (:gen-class)
  (:require
    [challenge-data-processing.question-01 :as question-01]
    [challenge-data-processing.question-02 :as question-02]
    [challenge-data-processing.question-03 :as question-03]
    [jsonista.core :as json]))


(defn -main
  [& _]
  (try
    (let [input (-> "lipstick.json"
                    slurp
                    json/read-value)]
      (println "Write answer to first.csv ...")
      (spit "first.csv" (question-01/answer input))
      (println "Write answer to second.csv ...")
      (spit "second.csv" (question-02/answer input))
      (println "Write answer to third.csv ...")
      (spit "third.json" (question-03/answer input))
      (println "Done."))
    (catch Throwable ex
      (println ex))))
