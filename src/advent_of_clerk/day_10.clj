;; # 🎄 Advent of Clerk: Day 10
(ns advent-of-clerk.day-10
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.string :as cstr]))


(def input (->> (utils/load-input "day_10.txt")
                (cstr/split-lines)))

;; Example input:
^{::clerk/visibility {:code :hide}}
(def ex-input
  ["addx 15"
   "addx -11"
   "addx 6"
   "addx -3"
   "addx 5"
   "addx -1"
   "addx -8"
   "addx 13"
   "addx 4"
   "noop"
   "addx -1"
   "addx 5"
   "addx -1"
   "addx 5"
   "addx -1"
   "addx 5"
   "addx -1"
   "addx 5"
   "addx -1"
   "addx -35"
   "addx 1"
   "addx 24"
   "addx -19"
   "addx 1"
   "addx 16"
   "addx -11"
   "noop"
   "noop"
   "addx 21"
   "addx -15"
   "noop"
   "noop"
   "addx -3"
   "addx 9"
   "addx 1"
   "addx -3"
   "addx 8"
   "addx 1"
   "addx 5"
   "noop"
   "noop"
   "noop"
   "noop"
   "noop"
   "addx -36"
   "noop"
   "addx 1"
   "addx 7"
   "noop"
   "noop"
   "noop"
   "addx 2"
   "addx 6"
   "noop"
   "noop"
   "noop"
   "noop"
   "noop"
   "addx 1"
   "noop"
   "noop"
   "addx 7"
   "addx 1"
   "noop"
   "addx -13"
   "addx 13"
   "addx 7"
   "noop"
   "addx 1"
   "addx -33"
   "noop"
   "noop"
   "noop"
   "addx 2"
   "noop"
   "noop"
   "noop"
   "addx 8"
   "noop"
   "addx -1"
   "addx 2"
   "addx 1"
   "noop"
   "addx 17"
   "addx -9"
   "addx 1"
   "addx 1"
   "addx -3"
   "addx 11"
   "noop"
   "noop"
   "addx 1"
   "noop"
   "addx 1"
   "noop"
   "noop"
   "addx -13"
   "addx -19"
   "addx 1"
   "addx 3"
   "addx 26"
   "addx -30"
   "addx 12"
   "addx -1"
   "addx 3"
   "addx 1"
   "noop"
   "noop"
   "noop"
   "addx -9"
   "addx 18"
   "addx 1"
   "addx 2"
   "noop"
   "noop"
   "addx 9"
   "noop"
   "noop"
   "noop"
   "addx -1"
   "addx 2"
   "addx -37"
   "addx 1"
   "addx 3"
   "noop"
   "addx 15"
   "addx -21"
   "addx 22"
   "addx -6"
   "addx 1"
   "noop"
   "addx 2"
   "addx 1"
   "noop"
   "addx -10"
   "noop"
   "noop"
   "addx 20"
   "addx 1"
   "addx 2"
   "addx 2"
   "addx -6"
   "addx -11"
   "noop"
   "noop"
   "noop"])

;; ---
;; ## Part 1

;; Some constants:
(def CPU {:cycles 0 :regX 1 :last-instr [:init 1]})
(def observe-cycles (set (take 6 (iterate (partial + 40) 20))))
(def screen-w 40)


(defn parse [input]
  (map #(let [[cmd n] (cstr/split % #"\s")
              cmd (keyword cmd)]
          (if (nil? n)
            [cmd]
            [cmd (parse-long n)])) input))

(def instruction->cmd first)
(def instruction->val #(nth % 1 0))

(defn exec-instruction [{:keys [cycles regX]} [cmd v :as instr]]
  (case cmd
    :noop {:cycles (inc cycles) :regX regX :last-instr instr}
    :addx {:cycles (+ 2 cycles) :regX (+ v regX) :last-instr instr}
    (throw (ex-info "Unknown command!" {:cmd cmd}))))

(defn run-procedure [procedure]
  (reductions
   exec-instruction
   CPU
   procedure))

(defn calc-signal-strength [cycles regX] (* cycles regX))

(defn observe-signal-strengths [observe-cycles cpu-states]
  (reduce (fn [signal-strengths {:keys [cycles regX last-instr]}]
            (if-let [cycles
                     (cond (some? (observe-cycles cycles)) cycles
                           (and (= :addx (instruction->cmd last-instr))
                                (some? (observe-cycles
                                        (dec cycles)))) (dec cycles)
                           :else nil)]
              (assoc signal-strengths
                     cycles
                     (calc-signal-strength
                      cycles (- regX (instruction->val last-instr))))
              signal-strengths))
          {}
          (rest cpu-states)))

(defn solve-1
  [input]
  (->> input
       parse
       run-procedure
       (observe-signal-strengths observe-cycles)
       vals
       (apply +)))

;; ### Studies

(def procedure (parse ex-input))

(exec-instruction CPU (first procedure))

(reduce exec-instruction
        CPU
        [[:noop] [:addx 3] [:addx -5]])

(def cpu-states (run-procedure procedure))
(rest cpu-states)

(def results (observe-signal-strengths observe-cycles cpu-states))

(apply + (vals results))


;; ### Observations
;; - since the value of the register doesn’t change during both cycles of `addx` but only after the second one, we don’t need to process the last instruction but we DO have to count the cycles

;; ---
;; ## Part 2

(defn draw? [x sprite-x]
  (<= (dec sprite-x) x (inc sprite-x)))

(defn get-sprite-coords [cpu-states]
  (reduce
   (fn [pos+draw? {:keys [cycles regX last-instr] :as cpu}]
     (let [n        (if (= :addx (instruction->cmd last-instr)) 2 1)
           sprite-x (- regX (instruction->val last-instr))]
       (apply conj pos+draw?
              (map #(let [this-cyc (- cycles %)
                          coords   [(mod (dec this-cyc) screen-w)
                                    (int (/ (dec this-cyc) screen-w))]]
                      [coords (draw? (first coords) sprite-x)])
                   (range (dec n) -1 -1)))))
   []
   (rest cpu-states)))

(defn solve-2
  [input]
  (->> input
       parse
       run-procedure
       get-sprite-coords))

;; ### Studies

(def sprite-coords (get-sprite-coords cpu-states))

(defn make-chart [data]
  (clerk/html
   (let [w screen-w
         h (inc (second (first (last data))))
         bgcol "#111827"]
     [:svg {:width (* 20 w)
            :style {"max-width" "100%"
                    "padding" "1rem"
                    "background-color" bgcol}
            :view-box (str "0 0 " w " " h)}
      (into [:g]
            (for [[[x y] draw?] data]
              (when draw?
                [:rect {:x x :y y :width 1 :height 1
                        :fill "white"
                        :stroke-width 0.02
                        :stroke bgcol}])))])))

(make-chart sprite-coords)

(def sprite-coords-input (get-sprite-coords (run-procedure (parse input))))

(make-chart sprite-coords-input)

;; ### Observations
;; - for `addx`, sprite position is only changed after both pixels have been drawn, so we only need to consider the previous position

(comment

  )

