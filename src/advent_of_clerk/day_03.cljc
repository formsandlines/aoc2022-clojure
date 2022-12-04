;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils]
            [clojure.set :as cset]
            [clojure.string :as str]))

(def input (->> (utils/load-input "day_03.txt")
                (str/split-lines)))

(def ex-input
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   "CrZsJsPPZsGzwwsLwLmpwMDw"])

;; ---
;; ## Part 1

(defn type->prio [c]
  (let [n (int c)]
    (if (> n 90)
      (- n 96)
      (+ 27 (- n 65)))))

(defn prio->type [n] ;; unnecessary, just for debugging
  (char (if (< n 27)
          (+ n 96)
          (+ 65 (- n 27)))))

(defn parse-input-1 [input]
  (let [split-half #(split-at (/ (count %) 2) %)]
    (map #(->> %
               (map type->prio)
               split-half
               (map set))
         input)))

(defn compartment-intersections [rucksacks]
  (map #(apply cset/intersection %)
       rucksacks))

(defn solve-1
  [input]
  (->> input
       parse-input-1
       compartment-intersections
       (map (partial reduce +))
       (reduce +)))

;; ### Studies

(clerk/table
 (clerk/use-headers (apply vector
                           ["Char" "Unicode" "Char→Priority" "Priority→Char"]
                           (mapv (fn [c] [c (int c) (type->prio c)
                                          (prio->type (type->prio c))])
                                 "azAZ"))))

(def rucksacks (parse-input-1 ex-input))

(def shared-prios (compartment-intersections rucksacks))

(reduce + (map (partial reduce +) shared-prios))

;; ### Observations
;; - order within compartments doesn’t matter
;; - the category symbols are irrelevant, only their priorities matter

;; ---
;; ## Part 2

(defn parse-input-2 [input]
  (partition 3 (map #(->> % (map type->prio) set)
                    input)))

(defn find-group-badges [inventory-groups]
  (map (comp first (partial apply cset/intersection))
       inventory-groups))

(defn solve-2
  [input]
  (->> input
       parse-input-2
       find-group-badges
       (reduce +)))

;; ### Studies

(def inventory-groups (parse-input-2 ex-input))

(apply cset/intersection (first inventory-groups))
(apply cset/intersection (second inventory-groups))

;; ### Observations
; - compartments are irrelevant here


(comment

  )


