(ns advent-of-clerk.day-03-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-03 :refer [solve-1 solve-2 ex-input input]]))


(deftest solve-1-test
  (testing "Part 1 example correct"
    (is (= (solve-1 ex-input) 157)))
  (testing "Part 1 correct"
    (is (= (solve-1 input) 7553)))) 

(deftest solve-2-test
  (testing "Part 2 example correct"
    (is (= (solve-2 ex-input) 70)))
  (testing "Part 2 correct"
    (is (= (solve-2 input) 2758))))

