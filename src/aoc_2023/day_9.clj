(ns aoc-2023.day-9
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_9.txt"))

;; Part 1

(def sample-input "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45")

(defn parse-input [input]
  (map #(->> (str/split % #" ")
             (map parse-long))
       (str/split-lines input)))

(defn differences [readings]
  (loop [readings' readings
         diffs []]
    (if-not (next readings')
      diffs
      (recur (rest readings')
             (conj diffs (- (second readings')
                            (first readings')))))))

(defn differences-sequences [reading-history]
  (loop [diff-seqs [reading-history]]
    (if (apply = (last diff-seqs))
      diff-seqs
      (recur (conj (vec diff-seqs)
                   (differences (last diff-seqs)))))))

(defn extrapolate [reading-history]
  (let [diff-seqs (reverse (differences-sequences reading-history))]
    (reduce (fn [x y]
              (conj (vec y) (+ (last y)
                               (last x))))
            diff-seqs)))

(->> input
     parse-input
     (map extrapolate)
     (map last)
     (reduce +))

;; Part 2

(->> input
     parse-input
     (map reverse)
     (map extrapolate)
     (map last)
     (reduce +))
