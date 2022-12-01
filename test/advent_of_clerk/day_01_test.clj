(ns advent-of-clerk.day-01-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-01 :refer [solve-1 solve-2]]))


(deftest solve-1-test
  (testing "Part 1 correct"
    (is (= (solve-1) true)))) 

(deftest solve-2-test
  (testing "Part 2 correct"
    (is (= (solve-2) true))))

