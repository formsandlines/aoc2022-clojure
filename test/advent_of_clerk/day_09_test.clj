(ns advent-of-clerk.day-09-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-09 :refer [solve-1 solve-2 ex-input input]]))


(deftest solve-1-test
  (testing "Part 1 example correct"
    (is (= (solve-1 (first ex-input)) 13)))
  (testing "Part 1 correct"
    (is (= (solve-1 input) 6745)))) 

(deftest solve-2-test
  (testing "Part 2 example correct"
    (is (= (solve-2 (first ex-input)) 1)))
  (testing "Part 2 example correct"
    (is (= (solve-2 (second ex-input)) 36)))
  (testing "Part 2 correct"
    (is (= (solve-2 input) 2793))))

