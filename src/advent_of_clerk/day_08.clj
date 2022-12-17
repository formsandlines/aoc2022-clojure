;; # 🎄 Advent of Clerk: Day 8
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.set :as cset]
            [clojure.string :as cstr]))


(def input (->> (utils/load-input "day_08.txt")
                (cstr/split-lines)))

(def ex-input
  ["30373"
   "25512"
   "65332"
   "33549"
   "35390"])

;; row number == column number:
(== (count input) (count (first input)))

;; ---
;; ## Part 1

(defn parse [input]
  (map #(map (comp parse-long str) %) input))

(defn make-grid [xss]
  (let [rows (map-indexed
              (fn [y row] (map-indexed (fn [x n] [[x y] n]) row))
              xss)
        cols (apply map vector rows)]
    [rows cols]))

(defn find-visible [[rows cols]]
  (loop [[lmax l2 & lr] cols [rmax r2 & rr] (reverse cols)
         [tmax t2 & tr] rows [bmax b2 & br] (reverse rows)
         visible (into {} (concat lmax rmax tmax bmax))]
    (let [sel-visible #(if (< (second %1) (second %2)) %2 %1)
          [lmax rmax
           tmax bmax] (map (partial apply map sel-visible)
                           [[lmax l2] [rmax r2] [tmax t2] [bmax b2]])
          visible (apply merge visible (into {} (concat lmax rmax tmax bmax)))]
      (if (empty? lr)
        visible
        (recur (cons lmax lr) (cons rmax rr)
               (cons tmax tr) (cons bmax br)
               visible)))))

(defn solve-1
  [input]
  (->> input
       parse
       make-grid
       find-visible
       count))

;; ### Studies

(def grid (make-grid (parse ex-input)))

(def visible-heights (find-visible grid))

(count visible-heights)


;; ### Observations
;; - it doesn’t matter from which direction a tree is visible to be visible
;; - visibility checks from each direction are independent from each other
;; - if a tree is marked as visible, it doesn’t need any further checks
;; - a tree can block a tree of the same height
;; - all edge-trees have to be counted since they are all visible

;; ---
;; ## Part 2

(defn calc-scenic-scores [[rows cols]]
  (let [init-heights (partial map (comp vector second))
        cons-heights (fn [xs acc-heights]
                       (map (fn [[_ n] heights] (cons n heights))
                            xs acc-heights))
        calc-score   (fn [self-height heights-in-dir]
                       (reduce (fn [sum height] (if (< height self-height)
                                                  (inc sum)
                                                  (reduced (inc sum))))
                               0 heights-in-dir))
        [min max] [0 (dec (count rows))]]
    (loop [[lhts l & lr] (cons (init-heights (first cols)) (rest cols))
           [rhts r & rr] (cons (init-heights (last cols)) (rest (reverse cols)))
           [thts t & tr] (cons (init-heights (first rows)) (rest rows))
           [bhts b & br] (cons (init-heights (last rows)) (rest (reverse rows)))
           coord->score {}]
      (let [scores (map (fn [xs xhts]
                          (into {} (map (fn [[[x y :as coords] n] heights]
                                          (when (and (> x min) (> y min)
                                                     (< x max) (< y max))
                                            [coords (calc-score n heights)]))
                                        xs xhts)))
                        [l r t b] [lhts rhts thts bhts])
            coord->score (apply merge-with * coord->score scores)]
        (if (< (count lr) 2) ;; omit first/last row/col
          coord->score
          (recur (cons (cons-heights l lhts) lr)
                 (cons (cons-heights r rhts) rr)
                 (cons (cons-heights t thts) tr)
                 (cons (cons-heights b bhts) br)
                 coord->score))))))

(defn solve-2
  [input]
  (->> input
       parse
       make-grid
       calc-scenic-scores
       vals
       (apply max)))

;; ### Studies

(def coord->score (calc-scenic-scores grid))

(apply max (vals coord->score))

;; ### Observations
;; - trees on the edges will always have a scenic score of 0, so they can be ignored
;; - seems like we cannot make use of the results of part 1

;; TIL:
;; - `for` might be more convenient to transform n-dimensional seqs than `map`
;; - transforming a 2D seq into a map of coordinates helps to quickly traverse a grid in all directions by simple coordinate manipulation and lookup in the map (see https://gitlab.com/maximoburrito/advent2022/-/blob/main/src/day08/main.clj for example)

;; exmaple:
(for [[i xs] (map-indexed vector (repeat 3 ['a 'b 'c]))
      [j x]  (map-indexed vector xs)]
  [[i j] x])


(comment

  )

