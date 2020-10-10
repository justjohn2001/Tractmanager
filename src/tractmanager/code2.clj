(ns tractmanager.code2
  (:require [clojure.string :as string]))

(def mat [["A" "O" "T" "D" "L" "R" "O" "W"]
          ["L" "C" "B" "M" "U" "M" "L" "U"]
          ["D" "R" "U" "J" "D" "B" "L" "J"]
          ["P" "A" "Z" "H" "Z" "Z" "E" "F"]
          ["B" "C" "Z" "E" "L" "F" "H" "W"]
          ["R" "K" "U" "L" "V" "P" "P" "G"]
          ["A" "L" "B" "L" "P" "O" "P" "Q"]
          ["B" "E" "M" "O" "P" "P" "J" "Y"]])

(defn count-words-in-matrix
  [mat word]
  (let [lines (string/join "-"
                           (concat (map (partial apply str) mat)
                                   (map (comp (partial apply str) reverse) mat)
                                   (map (partial apply str) (apply map list mat))
                                   (map (comp (partial apply str) reverse) (apply map list mat))))]
    (-> word
        re-pattern
        (re-seq lines)
        count)))
