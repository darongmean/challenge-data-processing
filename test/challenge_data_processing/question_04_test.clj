(ns challenge-data-processing.question-04-test
  (:require
    [challenge-data-processing.question-04 :as question-04]
    [clojure.test :refer [deftest testing is]]))


(deftest question-04-test
  (testing "Given a nested map,"
    (let [lipstick {"image" "http://example.org/img01"
                    "other1" "other1"
                    "test1" {"image" [1 2 3]}
                    "test2" {"other2" {"image" "http://example.org/img02"}}
                    "test3" [{"image" "http://example.org/img03"} {"other3" "other3"}]
                    "test4" {"image" [{"image" "http://example.org/img04"} {"other4" "other4"} {"image" "http://example.org/img05"}]}}]
      (testing "image-vals should find string value of image key"
        (is (= #{"http://example.org/img01" "http://example.org/img02" "http://example.org/img03"
                 "http://example.org/img04" "http://example.org/img05"}
               (question-04/image-vals lipstick))))

      (testing "file-names should extract the file names"
        (is (= #{"img01" "img02" "img03" "img04" "img05"}
               (-> lipstick
                   question-04/image-vals
                   question-04/file-names)))))))
