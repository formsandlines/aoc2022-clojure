;; # 🎄 Advent of Clerk: Day 6
(ns advent-of-clerk.day-06
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils]))


(def input (->> (utils/load-input "day_06.txt")))

(def ex-input
  ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
   "bvwbjplbgvbhsrlpgdmjqwftvncz"
   "nppdvjthqldpwncqszvftbrmjlhg"
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

;; ---
;; ## Part 1

(defn partition-till-detected [n s]
  (take-while (complement (partial apply distinct?))
              (partition n 1 s)))

(defn count-to-marker [n datastream]
  (+ n (count (partition-till-detected n datastream))))

(defn solve-1
  [input]
  (count-to-marker 4 input))

;; ### Studies

(def ex (ex-input 0))

(def before-distinct4 (partition-till-detected 4 ex))

(+ 4 (count before-distinct4))

;; ### Observations
;; - pretty trivial to solve with partition, but I wonder how it compares to conventional solutions performance-wise

;; ---
;; ## Part 2

; <definitions>

(defn solve-2
  [input]
  (count-to-marker 14 input))

;; ### Studies

(def before-distinct14 (partition-till-detected 14 ex))

(+ 14 (count before-distinct14))

;; ### Observations
;; - just a variation, I was hoping for something more interesting


(comment

  )

