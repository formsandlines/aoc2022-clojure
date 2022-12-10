(ns advent-of-clerk.day-06-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-06 :refer [solve-1 solve-2 ex-input input]]))


(deftest solve-1-test
  (testing "Part 1 examples correct"
    (is (= (solve-1 (ex-input 0)) 7))
    (is (= (solve-1 (ex-input 1)) 5))
    (is (= (solve-1 (ex-input 2)) 6))
    (is (= (solve-1 (ex-input 3)) 10))
    (is (= (solve-1 (ex-input 4)) 11)))
  (testing "Part 1 correct"
    (is (= (solve-1 input) 1198)))) 

(deftest solve-2-test
  (testing "Part 2 examples correct"
    (is (= (solve-2 (ex-input 0)) 19))
    (is (= (solve-2 (ex-input 1)) 23))
    (is (= (solve-2 (ex-input 2)) 23))
    (is (= (solve-2 (ex-input 3)) 29))
    (is (= (solve-2 (ex-input 4)) 26)))
  (testing "Part 2 correct"
    (is (= (solve-2 input) 3120))))

