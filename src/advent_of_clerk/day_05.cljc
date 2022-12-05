;; # 🎄 Advent of Clerk: Day 5
(ns advent-of-clerk.day-05
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils]
            [clojure.string :as cstr]))


(def input (->> (utils/load-input "day_05.txt")
                (cstr/split-lines)))

(def ex-input
  ["    [D]    "
   "[N] [C]    "
   "[Z] [M] [P]"
   " 1   2   3 "
   ""
   "move 1 from 2 to 1"
   "move 3 from 1 to 3"
   "move 2 from 2 to 1"
   "move 1 from 1 to 2"])


;; ---
;; ## Part 1

(defn parse-stacks [input]
  (let [uppercase (set (map char
                            (range (int \A) (inc (int \Z)))))]
    (->> input
         (map (comp (partial into {})
                    (partial map-indexed
                             (fn [i xs]
                               (let [ids (filter uppercase xs)]
                                 (when-not (empty? ids)
                                   [(inc i) ids]))))
                    (partial partition-all 4)))
         (apply merge-with concat))))

(defn parse-instructions [input]
  (map (comp vec
             (partial map parse-long)
             (partial re-seq #"\d+")) input))

(defn parse [input]
  (let [[setup instrs] (take-nth 2 (partition-by empty? input))]
    {:stacks (parse-stacks (butlast setup))
     :instrs (parse-instructions instrs)}))

(defn cratemover9000 [stacks [n from to]]
  (reduce (fn [stacks _]
            (-> stacks
                (update to #(cons (first (stacks from)) %))
                (update from rest)))
          stacks
          (range n)))

(defn run-instructions [crate-mover data]
  (reduce (fn [stacks instr] (crate-mover stacks instr))
          (:stacks data) (:instrs data)))

(defn top-crates [stacks]
  (->> stacks
       (sort-by first)
       (map (comp first second))
       (apply str)))

(defn solve-1
  [input]
  (->> input
       parse
       (run-instructions cratemover9000)
       top-crates))

;; ### Studies

(def data (parse ex-input))

(cratemover9000 {1 (list \D \C \M) 2 (list \N \Z)}
                [2 1 2])

(def final-stack
  (reduce (fn [stacks instr] (cratemover9000 stacks instr))
          (:stacks data) (:instrs data)))

(apply str (map (comp first second) (sort-by first final-stack)))


;; ### Observations
;; - when multiple crates are moved, their order will be reversed in the stack
;; - in the input, all crates are the same amount of chars apart
;; - I feel like parsing the input was the hardest part of the puzzle

;; TIL:
;; - `peek` and `pop` behave differently on vectors vs lists and throw on some special sequences… I replaced them with `first` and `rest` instead, but still have to learn how and why they would be used in Clojure
;; - could have used `(s/split #"\n\n")` instead of partition to split the input
;; - you can use pipes in regex-pattern for `re-seq` to split a sequence into alternative parts
;; - you can “transpose” across a seq of seqs by applying `map` to all of them, which would have helped with the initial stack input (a string is also a seq)

;; I really like this transpose-based parsing approach (adapted from [@motform](https://github.com/motform/advent-of-clojure/blob/master/src/advent_of_clojure/2022/05.clj)):
(let [[init _] (cstr/split (utils/load-input "day_05.txt") #"\n\n")
      transpose (fn [matrix] (apply map vector matrix))
      ->stack   (fn [xs] {(-> xs last str parse-long) (drop-last xs)})]
  (->> init
       cstr/split-lines
       (apply map vector)
       (map (partial remove #(#{\space \[ \]} %)))
       (remove empty?)
       (map ->stack)
       (apply merge)))

;; ---
;; ## Part 2

(defn cratemover9001 [stacks [n from to]]
  (-> stacks
      (update to #(concat (take n (stacks from)) %))
      (update from #(drop n %))))

(defn solve-2
  [input]
  (->> input
       parse
       (run-instructions cratemover9001)
       top-crates))

;; ### Studies

(cratemover9001 {1 (list \D \C \M) 2 (list \N \Z)}
                [2 1 2])

;; ### Observations
;; - no reverse ordering of moved stacks here
;; - of course, the first part could have been solved similarly without the `reduce` for iteration, but I like how it displays the slower one-by-one stack operation of the _CrateMover9000_ vs the fast all-at-once chunk moving of the _CrateMover9001_ :)


(comment

  )
