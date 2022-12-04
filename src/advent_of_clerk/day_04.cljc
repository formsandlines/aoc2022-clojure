;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.day-04
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils]
            [clojure.set :as cset]
            [clojure.string :as cstr]))


(def input (->> (utils/load-input "day_04.txt")
                (cstr/split-lines)))

(def ex-input
  ["2-4,6-8"
   "2-3,4-5"
   "5-7,7-9"
   "2-8,3-7"
   "6-6,4-6"
   "2-6,4-8"])


;; ---
;; ## Part 1

(defn parse [input]
  (map (comp (partial partition 2)
             (partial map parse-long)
             (partial re-seq #"\d+"))
       input))

(defn intervals-contained?
  [[[a1 a2] [b1 b2]]]
  (or (and (>= b1 a1) (<= b2 a2))
      (and (>= a1 b1) (<= a2 b2))))

(defn solve-1
  [input]
  (->> input
       parse
       (filter intervals-contained?)
       count))

;; ### Studies

(def interval-groups (parse ex-input))

(filter intervals-contained? interval-groups)


;; ### Observations
;; - which contains which is irrelevant
;; - could be done with set operations but maybe too inefficient

;; ---
;; ## Part 2

(defn intervals-overlap?
  [[[a1 a2] [b1 b2]]]
  (or (and (>= b1 a1) (<= b1 a2))
      (and (>= a1 b1) (<= a1 b2))))

(defn solve-2
  [input]
  (->> input
       parse
       (filter intervals-overlap?)
       count))

;; ### Studies

(filter intervals-overlap? interval-groups)

;; ### Observations
;; - the number of overlapping sections doesn’t matter


(comment

  )


