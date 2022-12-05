;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.day-04
  (:require #?@(:bb [] :clj [[nextjournal.clerk :as clerk]])
            [advent-of-clerk.utils :as utils :refer [not-bb]]
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

(defn get-bounds [intervals]
  (reduce (fn [[lower-min upper-max] [lower upper]]
            [(if (< lower lower-min) lower lower-min)
             (if (> upper upper-max) upper upper-max)])
          [##Inf 0] intervals))

(defn get-intersection
  [[[a1 a2] [b1 b2] :as group]]
  (when (intervals-overlap? group)
    [(max a1 b1) (min a2 b2)]))

(defn make-chart [groups]
  (let [[x-min x-max] (get-bounds (map get-bounds groups))
        unit-h 0.3
        margin 0.1
        amount  (count groups)
        chart-h (+ (* amount unit-h)
                   (* (dec amount) margin))]
    [:svg {:width "100%"
           :view-box (str "0 0 " (inc x-max) " " chart-h)
           :style {:margin "2rem 0"}}
     (into [:g]
           (map-indexed
            (fn [y [[a1 a2] [b1 b2] :as group]]
              (into [:g]
                    (map-indexed
                     (fn [i [x1 x2]]
                       [:rect {:x (dec x1)
                               :y (* y (+ unit-h margin))
                               :width (inc (- x2 x1))
                               :height unit-h
                               :fill (cond
                                       (== i 3) "#ff0076"
                                       (== i 2) "#344a77"
                                       (== i 1) "#253455"
                                       :else "#090c14")}])
                     (let [overlap (get-intersection group)
                           bg [1 x-max]]
                       (if overlap
                         (concat [bg] group [overlap])
                         (concat [bg] group))))))
            groups))]))

(not-bb
 (clerk/html (make-chart interval-groups)))

(not-bb
 (clerk/html (make-chart (parse input))))

;; ### Observations
;; - the number of overlapping sections doesn’t matter
;; - no interesting pattern in the overlap-viz :(


(comment

  )


