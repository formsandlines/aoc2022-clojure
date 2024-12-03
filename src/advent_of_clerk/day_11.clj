;; # 🎄 Advent of Clerk: Day 11
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.set :as cset]
            [clojure.string :as cstr]))

(def input (->> (utils/load-input "day_11.txt")
                (cstr/split-lines)))

(def ex-input
  ["Monkey 0:"
   "  Starting items: 79, 98"
   "  Operation: new = old * 19"
   "  Test: divisible by 23"
   "    If true: throw to monkey 2"
   "    If false: throw to monkey 3"
   ""
   "Monkey 1:"
   "  Starting items: 54, 65, 75, 74"
   "  Operation: new = old + 6"
   "  Test: divisible by 19"
   "    If true: throw to monkey 2"
   "    If false: throw to monkey 0"
   ""
   "Monkey 2:"
   "  Starting items: 79, 60, 97"
   "  Operation: new = old * old"
   "  Test: divisible by 13"
   "    If true: throw to monkey 1"
   "    If false: throw to monkey 3"
   ""
   "Monkey 3:"
   "  Starting items: 74"
   "  Operation: new = old + 3"
   "  Test: divisible by 17"
   "    If true: throw to monkey 0"
   "    If false: throw to monkey 1"])

;; ---
;; ## Part 1

(defn read-data
  [[id, items, a op b, test-or-if bool n]]
  (cond
    (some? id) (parse-long id)
    (some? items) (mapv bigint (cstr/split items #", "))
    (some? op) (let [read-term #(if (= "old" %) % (parse-long %))]
                 [(eval (read-string op)) (read-term a) (read-term b)])
    (= "Test" test-or-if) (parse-long n)
    (= "If " test-or-if) (parse-long n)))

(defn make-op [op a b]
  (let [arg (gensym "x")
        make-term #(if (= % "old") arg %)]
    (eval `(fn [~arg] (~op ~(make-term a) ~(make-term b))))))

(defn parse-monkey-data
  [[id items [op a b] test-cond test-true test-false]]
  {:id id
   :items items
   :operation (make-op op a b)
   :test #(if (== 0 (rem % test-cond)) test-true test-false)
   :inspections 0})

(defn parse [input]
  (->> input
       (partition-all 7)
       (mapv (fn [xs]
               (->> xs
                    (map #(->> %
                               (re-find #"Monkey (\d+)|items: (.+$)|Operation.+?= (\S+) ([*+/-]) (\S+)|(If |Test)(true|false)?.+?(\d+)")
                               rest
                               read-data))
                    parse-monkey-data)))))

(defn throw-items [monkeys throws]
  (mapv (fn [monkey]
          (update monkey
                  :items #(if-let [items (throws (:id monkey))]
                            (concat % items)
                            %)))
        monkeys))

(def relief #(bigint (/ % 3)))

(defn play-round [relief-fn monkeys]
  (loop [i 0, monkeys monkeys]
    (let [{:keys [inspections items operation test]
           :as monkey} (monkeys i)
          throws (reduce (fn [receivers item]
                           (let [worry-lvl (relief-fn (operation item))]
                             (update receivers
                                     (test worry-lvl)
                                     #(if (nil? %) [worry-lvl]
                                          (conj % worry-lvl)))))
                         {}
                         items)
          monkey (-> monkey
                     (update :inspections (partial + (count items)))
                     (assoc :items []))
          monkeys (-> monkeys
                      (assoc i monkey)
                      (throw-items throws))
          i (inc i)]
      (if (< i (count monkeys))
        (recur i monkeys)
        monkeys))))

(defn monkey-business [monkeys]
  (->> monkeys
       (map :inspections)
       (sort >)
       (take 2)
       (reduce *)))


(defn solve-1
  [input]
  (->> input
       parse
       (iterate (partial play-round relief))
       (take (inc 20))
       last
       monkey-business))


;; ### Studies

(def monkey1 {:items [79 98]
              :operation #(* 19 %)
              :test #(== 0 (rem % 23))
              :inspections 0})

(def monkeys (parse ex-input))

(def read-example
  (map read-data
       [["0" nil nil nil nil nil nil nil]
        [nil "79, 98" nil nil nil nil nil nil]
        [nil nil "old" "*" "19" nil nil nil]
        [nil nil nil nil nil "Test" nil "23"]
        [nil nil nil nil nil "If " "true" "2"]
        [nil nil nil nil nil "If " "false" "3"]]))

(def monkey-example
  (parse-monkey-data read-example))

((make-op (read-string "*") 19 "old") 2)

((:test monkey-example) 46)
((:operation monkey-example) 2)

(play-round relief monkeys)

(def round20 (last (take 21 (iterate (partial play-round relief) monkeys))))

(def inspections (sort > (map :inspections round20)))

(reduce * (take 2 inspections))


;; ### Observations

;; - in each round, items need to be assigned to or detached from monkeys and the number of items being inspected by a monkey must be counted
;; - each monkey might be best represented as a map

;; ---
;; ## Part 2

; <definitions>

(defn solve-2
  [input]
  (->> input
       parse
       (iterate (partial play-round relief))
       (take (inc 300))
       #_last
       #_monkey-business))

(solve-2 ex-input)

;; ### Studies
; <explore data>

;; ### Observations
; <write notes>


(comment

  

  )

