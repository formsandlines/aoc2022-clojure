;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils]
            [clojure.string :as str]))


(def input (->> (utils/load-input "day_01.txt")
                (str/split-lines)
                (map parse-long)))

(def ex-input
    [1000
     2000
     3000
     nil
     4000
     nil
     5000
     6000
     nil
     7000
     8000
     9000
     nil
     10000])

(defn parse-groups
  [input]
  (->> input
       (partition-by #(some? %))
       (remove #(= '(nil) %))))

(defn sum-groups
  [groups]
  (map (partial apply +) groups))

(def groups (parse-groups ex-input))
(def sums (sum-groups groups))
(apply max sums)


(defn solve-1
  [input]
  (apply max (->> input
                  parse-groups
                  sum-groups)))

(defn solve-2
  [input]
  nil)


(comment




  )
