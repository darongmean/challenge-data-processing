(ns challenge-data-processing.core
  (:gen-class)
  (:require
    [challenge-data-processing.question-01 :as question-01]
    [jsonista.core :as json]))


(defn -main
  [& _]
  (try
    (let [input (-> "lipstick.json"
                    slurp
                    json/read-value)]
      (println "Write answer to first.csv ...")
      (spit "first.csv" (question-01/answer input))
      (println "Done."))
    (catch Throwable ex
      (println ex))))
