;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02-logic
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils]
            [clojure.test :as t :refer [deftest is testing]]
            [clojure.core.logic :as l :refer [run* defne]]
            [clojure.string :as s]))


(def input (->> (utils/load-input "day_02.txt")
                (s/split-lines)))

(def ex-input
  ["A Y"
   "B X"
   "C Z"])

;; ---
;; ## Part 1

(def WIN 6) (def LOSE 0) (def DRAW 3)
(def ROCK 1) (def PAPER 2) (def SCISSORS 3)

(def choice->score {\A ROCK \B PAPER \C SCISSORS
                    \X ROCK \Y PAPER \Z SCISSORS})

(def input->outcome {\X LOSE \Y DRAW \Z WIN})

(defn parse-input [char->score input]
  (map (comp (partial map char->score)
             (partial take-nth 2)) input))

#_:clj-kondo/ignore
(defne win [pl op]
  ([1 3])  ;; ROCK vs SCISSORS
  ([2 1])  ;; PAPER vs ROCK
  ([3 2])) ;; SCISSORS vs PAPER

#_:clj-kondo/ignore
(defne draw [pl op]
  ([x x] (l/membero x [ROCK PAPER SCISSORS])))

#_:clj-kondo/ignore
(defne outcome [pl op out]
  ([_ _ 6] (win pl op))  ;; WIN
  ([_ _ 3] (draw pl op)) ;; DRAW
  ([_ _ 0] (win op pl))) ;; LOSE

(defn eval-round
  [[op pl]]
  (+ pl
     (first (run* [q] (outcome pl op q)))))

(defn solve-1
  [input]
  (->> input
       (parse-input choice->score)
       (map eval-round)
       (reduce +)))

(deftest solve-1-test
  (testing "Part 1 example correct"
    (is (= (solve-1 ex-input) 15)))
  (testing "Part 1 correct"
    (is (= (solve-1 input) 11873))))

;; ### Studies

(eval-round [PAPER ROCK])

(eval-round [ROCK PAPER])

(eval-round [ROCK ROCK])

(def rounds (parse-input choice->score ex-input))

(map eval-round rounds)

;; ---
;; ## Part 2

(defn eval-strategy
  [[op out]]
  (+ out
     (first (run* [q] (outcome q op out)))))

(defn solve-2
  [input]
  (->> input
       (parse-input (merge choice->score input->outcome))
       (map eval-strategy)
       (reduce +)))

(deftest solve-2-test
  (testing "Part 2 example correct"
    (is (= (solve-2 ex-input) 12)))
  (testing "Part 2 correct"
    (is (= (solve-2 input) 12014))))

;; ### Studies

(run* [q] (outcome q SCISSORS LOSE))

(run* [q] (win q SCISSORS))
(run* [q] (win ROCK q))

(run* [q] (draw SCISSORS q))


(eval-strategy [ROCK DRAW])

(eval-strategy [PAPER LOSE])

(eval-strategy [SCISSORS WIN])

(def strategies (parse-input (merge choice->score input->outcome)
                             ex-input))

(map eval-strategy strategies)

;; ### Observations

;; - with logic programming, we can reuse the same relation for both parts of day 2, the only difference is in the binding of the lvars
;; - I prefer this approach in terms of clarity and simplicity, although it may not be as performant and introduces a dependency
;; - surely the relations can be further simplified and it would be nice if I could use my var-constants in patterns to avoid hard-coding the numbers


