(ns advent-of-clerk.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-input [filename]
  (slurp (io/resource filename)))

;; Helper for conditional babashka code without using reader conditionals
;; by @borkdude: https://twitter.com/borkdude/status/1599067149187764224
(defmacro if-bb
  "Execute `then` branch in babashka, or `else` branch in Clojure."
  [then else]
  (if (System/getProperty "babashka.version")
    then else))

(comment
  (if-bb
   (println "I’m in bb")
   (println "I’m in Clojure")))
