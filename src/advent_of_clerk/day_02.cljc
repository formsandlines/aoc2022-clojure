;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
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

(def choice->score {\A 1 \B 2 \C 3
                    \X 1 \Y 2 \Z 3})

(defn parse-input [char->score input]
  (map (comp (partial map char->score)
             (partial take-nth 2)) input))

(defn round->outcome [[player opponent]]
  (if (== player opponent)
    DRAW
    (case [player opponent]
      ([2 1] [3 2] [1 3]) WIN
      LOSE)))

(defn eval-rounds [rounds]
  (map (fn [[op pl]]
         (+ pl (round->outcome [pl op])))
       rounds))

(defn solve-1
  [input]
  (->> input
       (parse-input choice->score)
       eval-rounds
       (reduce +)))

;; ### Studies

(def rounds (parse-input choice->score ex-input))

(eval-rounds rounds)

;; ### Observations
;; - choice symbols are arbitrary and can be mapped 1:1 to their score
;; - rounds are independent, so order doesn’t matter
;; - successor number beats predecessor in a cycle 1 < 2 < 3 < 1 < …, but its not transitive: 1 > 3 (rock beats scissors)

;; TIL:
;; - `(into {} …)` doesn’t take lists like `'((1 2) (2 3))`
;; - since the input only has single characters, could have used destructuring on strings to parse the input


;; ---
;; ## Part 2

(def input->outcome {\X LOSE \Y DRAW \Z WIN})

(def opponent+outcome->player
  (into {} (for [pl [1 2 3]
                 op [1 2 3]]
             [[op (round->outcome [pl op])] pl])))

(defn eval-strategies [strategies]
  (map (fn [[op outcome]]
         (+ outcome (opponent+outcome->player [op outcome])))
       strategies))

(defn solve-2
  [input]
  (->> input
       (parse-input (merge choice->score input->outcome))
       eval-strategies
       (reduce +)))

;; ### Studies

(def strategies (parse-input (merge choice->score input->outcome)
                             ex-input))

(eval-strategies strategies)

;; ### Observations
;; - now the symbols in the 2. column map to win/draw/lose scores
;; - this is just a different equation in the _player-opponent-outcome_ relation, so maybe logic programming would be a natural fit for this puzzle?

;; TIL:
;; - relations could also be hard-coded as data using triples: [player opponent outcome]
;; - you can hide Clerk results using `^{::clerk/visibility {:result :hide}}`
;; - of course, an explicit lookup-table could do the job much more efficiently, but it isn’t really fun, so I try to avoid it


(comment

  )
