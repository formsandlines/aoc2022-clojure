;; # 🎄 Advent of Clerk: Day 9
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.string :as cstr]))


(def input (->> (utils/load-input "day_09.txt")
                (cstr/split-lines)))

(def ex-input
  [["R 4"
    "U 4"
    "L 3"
    "D 1"
    "R 4"
    "D 1"
    "L 5"
    "R 2"]

   ["R 5"
    "U 8"
    "L 8"
    "D 3"
    "R 17"
    "D 10"
    "L 25"
    "U 20"]])

;; ---
;; ## Part 1

(def translate {:L [-1  0]
                :R [ 1  0]
                :U [ 0  1]
                :D [ 0 -1]})

(defn parse [input]
  (map #(let [[dir steps] (cstr/split % #"\s")]
          [(keyword dir) (parse-long steps)]) input))

(defn move [dir [x y]]
  (let [[tx ty] (translate dir)]
    [(+ x tx) (+ y ty)]))

(defn follow [[hx hy] [tx ty :as tail]]
  (let [[dx    dy]    [(- hx tx) (- hy ty)]
        [xdist ydist] [(abs dx) (abs dy)]
        [xdir  ydir]  [(if (pos? dx) :R :L) (if (pos? dy) :U :D)]]
    (cond
      (and (<= xdist 1) (<= ydist 1)) tail  ;; close prox. -> no movement
      (== xdist 0) (move ydir tail)         ;; vertical movement
      (== ydist 0) (move xdir tail)         ;; horizontal movement
      :else (move xdir (move ydir tail))))) ;; diagonal movement

(defn process-instruction [init-state [dir steps]]
  (take (inc steps)
        (iterate
         (fn [{:keys [head tail]}]
           (let [head (move dir head)
                 tail (reverse
                       (reduce (fn [[lead & _ :as tail] x]
                                 (cons (follow lead x) tail))
                               (list (follow head (first tail)))
                               (rest tail)))]
             {:head head :tail tail}))
         init-state)))

(defn run-simulation [knotcount procedure]
  (reduce (fn [results instruction]
            (concat results
                    (rest (process-instruction (last results) instruction))))
          [{:head [0 0]
            :tail (repeat (dec knotcount) [0 0])}]
          procedure))

(defn solve-1
  [input]
  (->> input
       parse
       (run-simulation 2)
       (map (comp first :tail))
       distinct
       count))

;; ### Studies

;; We need a way to memorize positions already visited such that they will not be counted twice.

;; A coordinate system with (0,0) as the first (starting) position would be the natural choice.

;; From (0,0), we can move in all 4 directions:

[(move :L [0 0])
 (move :R [0 0])
 (move :U [0 0])
 (move :D [0 0])]

;; Diagonals are just juxtapositions of 2 orthogonal moves:

(move :L (move :U [0 0]))

;; The distance and angle between head and tail determines how the tail will follow to remain in close proximity:

[(follow [3 1] [1 1])
 (follow [1 1] [1 3])
 (follow [2 3] [1 1])
 (follow [3 2] [1 1])
 (follow [-2 3] [-1 1])
 (follow [-3 2] [-1 1])
 (follow [2 -3] [1 -1])
 (follow [3 -2] [1 -1])
 (follow [-2 -3] [-1 -1])
 (follow [-3 -2] [-1 -1])]

;; By applying `move` to the head on each step of the procedure and letting the tail `follow` it, their coordinates can then be determined:

(def procedure (parse (first ex-input)))

(def results1 (run-simulation 2 procedure))

;; Finally, counting all distinct coordinates from the tail results is trivial:

(count (distinct (map (comp first :tail) results1)))

;; TIL:
;; - `reductions` can help a lot and I should think about it more often
;; - use `(mapv + v1 v2)` to quickly add two vectors of the same dimension
;; - `Long/signum` exists (Java only)
;; - should use defaults with (implicit) `get` on maps/sets more often


;; ---
;; ## Part 2

(defn solve-2
  [input]
  (->> input
       parse
       (run-simulation 10)
       (map (comp last :tail))
       distinct
       count))

;; ### Studies

;; I extended `run-simulation` and `process-instruction` to work with tails of multiple knots, which just means that each preceeding knot in the list is the lead/head that the next one will follow:

(process-instruction {:head [1 1]
                      :tail (list [0 0])}
                     [:U 2])

(process-instruction {:head [0 0]
                      :tail (list [0 0] [0 0] [0 0])}
                     [:L 4])

;; ### Visualization

;; To visualize the results in a grid, it is important to recognize that the values can be negative and that the y-axis is flipped from top-down coordinates used by SVG. I made use of the view-box and SVG transform to translate the values to the correct coordinates.

(defn get-bounds [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    [[(apply min xs) (apply max xs)]
     [(apply min ys) (apply max ys)]]))

(defn make-chart [data & {:keys [head? knot2? knot10? traces?]
                          :or {head? true knot2? false knot10? false
                               traces? false}}]
  (clerk/html
   (let [data (map #(cons (:head %) (:tail %)) data)
         [[x-min x-max]
          [y-min y-max]] (get-bounds (apply concat data))
         w (inc (abs (- x-max x-min)))
         h (inc (abs (- y-max y-min)))
         bgcol "#111827"]
     [:svg {:width (* w 26)
            :style {"max-width" "100%"
                    "padding" "1rem"
                    "background-color" bgcol}
            :view-box (str x-min " " y-min " " w " " h)}
      [:g {:transform (str "translate(0," (+ y-min y-max 1) ") "
                           "scale(1, -1)")}
       (into [:g]
             (for [[head & tail] data
                   [i knot] (map-indexed vector (cons head tail))]
               [:g
                (when traces?
                  [:rect {:x (first knot) :y (second knot)
                          :width 1 :height 1
                          :fill (let [v (- 1.0 (float (/ i (count tail))))
                                      r (max (* v 150) 26)]
                                  (str "rgba(" r ", 43, 76," v ")"))
                          :stroke-width 0.02
                          :stroke bgcol}])
                (when (or (and head? (== i 0))
                          (and knot2? (== i 1))
                          (and knot10? (== i 9)))
                  [:circle {:cx (+ 0.5 (first knot)) :cy (+ 0.5 (second knot))
                            :r 0.25
                            :fill (cond
                                    (== i 0) "rgb(255, 255, 255)"
                                    (== i 9) "rgb(0, 66, 189)"
                                    (== i 1) "rgb(255, 100, 100)")}])]))]])))

;; The results of the first example with only the head-knot (white dot):
(make-chart results1)

;; And its characteristic pattern left behind by the tail-knot (red):
(make-chart results1 :head? false :knot2? true)

;; For 10 knots, we can show the traces as a gradient from red→blue with a blue dot emphasizing the last tail-knot:

(def results2 (run-simulation 10 procedure))
(make-chart results2 :head? false :knot2? true :knot10? true :traces? true)

;; The second example looks somewhat more interesting.

(def results3 (run-simulation 10 (parse (second ex-input))))

;; First, only the movement of the head:

(make-chart results3)

;; Let’s see how the tail follows:

(make-chart results3 :traces? true :knot2? true :knot10? true)

(count (distinct (map (comp last :tail) results3)))

;; While the head moves in a rectangular fashion, the tail tries to catch it with diagonal shortcuts, which of course reflects the encoded movement pattern.


;; ---

;; The actual input looks a lot like a random walk; uncomment the following lines if you are curios (takes a few seconds to render):

; (def results4 (run-simulation 2 (parse input)))

; (count (distinct (map (comp last :tail) results4)))

; (make-chart results4)

;; _Can’t render this with 10 knots, because of stack overflow. D: Would have to recreate this in canvas._


(comment

  )

