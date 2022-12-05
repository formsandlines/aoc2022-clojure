(ns advent-of-clerk.day-05-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-05 :refer [solve-1 solve-2 ex-input input]]))


(deftest solve-1-test
  (testing "Part 1 example correct"
    (is (= (solve-1 ex-input) "CMZ")))
  (testing "Part 1 correct"
    (is (= (solve-1 input) "MQSHJMWNH")))) 

(deftest solve-2-test
  (testing "Part 2 example correct"
    (is (= (solve-2 ex-input) "MCD")))
  (testing "Part 2 correct"
    (is (= (solve-2 input) "LLWJRBHVZ"))))

