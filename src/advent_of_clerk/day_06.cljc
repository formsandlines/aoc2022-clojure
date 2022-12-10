;; # 🎄 Advent of Clerk: Day 6
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
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

;; TIL:
;; - could also have used `cstr/index-of` to get the index of the first distinct subseq
;; - could have been solved more procedurally by using `(take n …)` to successively divide the string into n-parts, checking for distinctness on each division and continuing with `(drop n …)`

;; this would have had the same effect as (partition 4 1 …):

(map vector ex (drop 1 ex) (drop 2 ex) (drop 3 ex))

;; ---
;; ## Part 2

(defn solve-2
  [input]
  (count-to-marker 14 input))

;; ### Studies

(def before-distinct14 (partition-till-detected 14 ex))

(+ 14 (count before-distinct14))

;; ### Observations
;; - just a variation, I was hoping for something more interesting


;; ## Learning about how queues work in Clojure



;; Queues can be created form other sequentials
(into clojure.lang.PersistentQueue/EMPTY [1 2 3])

;; Queues are `sequential?` but not `seq?`:
(seq? clojure.lang.PersistentQueue/EMPTY)
(sequential? clojure.lang.PersistentQueue/EMPTY)

;; But they can be coerced to a `seq`:

(seq clojure.lang.PersistentQueue/EMPTY)
(seq (conj (conj clojure.lang.PersistentQueue/EMPTY 1) 2))


;; Thus, queues can be manipulated with sequential operations:
(filter (partial <= 2) (into clojure.lang.PersistentQueue/EMPTY [1 2 3]))

;; With `conj`, `pop` and `peek`, queues behave as expected: _first-in-first-out_

;; Both `conj` and `pop` retain the `PersistentQueue` type:

(type (conj clojure.lang.PersistentQueue/EMPTY 1))
(type (pop (conj clojure.lang.PersistentQueue/EMPTY 1)))

;; To see how queues behave different from vectors and lists in this regard:

(clerk/table
 (let [que1 (conj (clojure.lang.PersistentQueue/EMPTY) 'a)
       que2 (conj que1 'b)
       que3 (conj que2 'c)
       vec1 (conj [] 'a)
       vec2 (conj vec1 'b)
       vec3 (conj vec2 'c)
       lst1 (conj (list) 'a)
       lst2 (conj lst1 'b)
       lst3 (conj lst2 'c)]
   (clerk/use-headers
    (into [["" "conj" "pop" "peek"]]
          (map (fn [[t & xs]]
                 [t
                  xs
                  (map pop xs)
                  (map peek xs)])
               [["Queue" que1 que2 que3]
                ["Vector" vec1 vec2 vec3]
                ["List" lst1 lst2 lst3]])))))

;; Notes:
;; - a vector behaves like a stack
;; - a list behaves like a reversed stack
;; - queues also work in Babashka


(comment

  )

