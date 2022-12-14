;; # 🎄 Advent of Clerk: Day 7
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.set :as cset]
            [clojure.string :as cstr]))


(def input (->> (utils/load-input "day_07.txt")
                (cstr/split-lines)))

(def ex-input
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

;; ---
;; ## Part 1

(defn parse [lines]
  (let [parts (partition-by #(= (first %) \$) lines)
        split-by-space #(cstr/split % #" ")
        parse-file (fn [[x y]] (case x "dir" y (parse-long x)))
        parts->groups
        (fn [p1 p2]
          (let [cmds (butlast
                      (map (comp first rest rest split-by-space) p1))
                files (map (comp parse-file split-by-space) p2)]
            [cmds files]))]
    (map parts->groups
         (take-nth 2 parts)
         (take-nth 2 (rest parts)))))

(def dir? string?)

(defn build-paths [dir-groups]
  (let [remove-segment (fn [path] (cstr/replace path #"[^\/]+?\/$" ""))
        add-segment    (fn [path segm] (str path segm "/"))]
    (second
     (reduce (fn [[curr-path dirs] [segments contents]]
               (let [new-path (reduce (fn [path segm]
                                        (case segm
                                          ".." (remove-segment path)
                                          "/"  "/"
                                          (add-segment path segm)))
                                      curr-path segments)
                     contents (map #(if (dir? %)
                                      (add-segment new-path %)
                                      %) contents)]
                 [new-path (assoc dirs new-path contents)]))
             ["" {}]
             dir-groups))))

(defn calc-dirsizes
  [path known-sizes dirlist]
  (when-let [contents (dirlist path)]
    (let [[sz known-sizes]
          (reduce (fn [[this-size known-sizes] x]
                    (if (dir? x)
                      (if-let [sz (known-sizes x)]
                        [(+ this-size sz) known-sizes]
                        (let [known-sizes (calc-dirsizes x known-sizes dirlist)
                              sz (known-sizes x)]
                          [(+ this-size sz) known-sizes]))
                      [(+ this-size x) known-sizes]))
                  [0 known-sizes]
                  contents)]
      (assoc known-sizes path sz))))

(defn solve-1
  [input]
  (let [dir->size (->> input
                       parse
                       build-paths
                       (calc-dirsizes "/" {}))]
    (reduce + (filter (partial >= 100000) (vals dir->size)))))

;; ### Studies

;; How can this information be structured?

;; (a) json-like tree
;; - more accurate representation
;; - lots of overhead with deeply nested dirs

{:type :dir
 :name "/"
 :content
 [{:type :dir
   :name "e"
   :content
   [{:type :file
     :name "i"
     :size 584}]}
  {:type :file
     :name "f"
     :size 29116}]}

;; (b) vector tree (hickup style)
;; - simpler
;; - still hard to get at information for a specific directory

["/" ["e" 584] 29116]

;; (c) referenced graph
;; - flat structure
;; - quick and easy to look up information
;; - references have to be resolved (recursively)

{"/" ["e" 29116]
 "e" [584]}

;; #### Parsing

;; The terminal output can be divided into groups of:
;; 1. one or more `cd` commands followed by one `ls` command
;; 2. directory contents -> either directories or files
;; - assumption: `cd` only ever has one argument with no spaces in between

(def xs (partition-by #(= (first %) \$) ex-input))

;; The only relevant information is the directory names and the filesizes:

(def groups (parse ex-input))

;; For approach (c), the directory names have to be turned into absolute paths, since there could be multiple directories with the same name.

;; A path can now act as a key for the contents of its corresponding directory:

(def dirs (build-paths groups))

;; The paths-graph can then be walked completely, summing up directory sizes along the way:

(def dir->size (calc-dirsizes "/" {} dirs))

;; ### Observations
;; - in the end, the tree-walk is still necessary and it might have been more efficient to compute the sizes while parsing without having to build the absolute paths, but I wanted a flexible approach that is easier to reason about

;; TIL:
;; - you don’t even need to consider subdirectories if you just update all sizes of directories below the current dir for each file size
;; (-> see @Apple’s solution in Clojurian Slack `#adventofcode`, which also features a very nice regex to split the data)

;; ---
;; ## Part 2

(def disk-space 70000000)
(def update-space 30000000)

(defn solve-2
  [input]
  (let [dir->size (->> input
                       parse
                       build-paths
                       (calc-dirsizes "/" {}))
        free-space (- disk-space (dir->size "/"))
        space-required (- update-space free-space)]
    (some #(when (>= % space-required) %)
          (sort (vals dir->size)))))

;; ### Studies

;; With the `dir->size` map generated in part 1, the second part is trivial:

(def free-space (- disk-space (dir->size "/")))

(def space-required (- update-space free-space))

(some #(when (>= % space-required) %) (sort (vals dir->size)))

;; ### Observations
;; - directory names are irrelevant here, so we don’t need to sort the whole map


(comment

  )

