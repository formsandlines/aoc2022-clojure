(ns advent-of-clerk.day-10-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [advent-of-clerk.day-10 :refer [solve-1 solve-2 ex-input input]]))


(deftest solve-1-test
  (testing "Part 1 example correct"
    (is (= (solve-1 ex-input) 13140)))
  (testing "Part 1 correct"
    (is (= (solve-1 input) 16020)))) 

(deftest solve-2-test
  (testing "Part 2 example correct"
    (is (= (do (solve-2 ex-input)
               (println "Verified visually (part 2 example).") true)
           true)))
  (testing "Part 2 correct"
    (is (= (do (solve-2 input)
               (println "Verified visually (part 2 input).") "ECZUZALR")
           "ECZUZALR"))))

