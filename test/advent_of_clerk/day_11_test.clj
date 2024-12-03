(ns advent-of-clerk.day-11-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-11 :refer [solve-1 solve-2 ex-input input]]))


(deftest solve-1-test
  (testing "Part 1 example correct"
    (is (= (solve-1 ex-input) 10605)))
  (testing "Part 1 correct"
    (is (= (solve-1 input) 56120)))) 

(deftest solve-2-test
  (testing "Part 2 example correct"
    (is (= (solve-2 ex-input) 2713310158)))
  (testing "Part 2 correct"
    (is (= (solve-2 input) true))))

