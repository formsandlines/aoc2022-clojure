(ns advent-of-clerk.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-input [filename]
  #?(:clj (slurp (io/resource filename))))

;; Helper for conditional babashka code without using reader conditionals
;; by @borkdude: https://twitter.com/borkdude/status/1599067149187764224
(defmacro if-bb
  "Execute `then` branch in babashka, or `else` branch in Clojure."
  [then else]
  (if (System/getProperty "babashka.version")
    then else))

;; Derived from if-bb
(defmacro not-bb
  "Execute `body` when not in babashka."
  [body]
  (when-not (System/getProperty "babashka.version")
    body))

(comment
  (if-bb
   (println "I’m in bb")
   (println "I’m in Clojure"))
  
  (not-bb (println "I’m in Clojure")))
