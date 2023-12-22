(ns aoc-2023.day-6
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_6.txt"))

;; Part 1

(def sample-input "Time:      7  15   30\nDistance:  9  40  200")

(defn parse-input [input]
  (let [[times distances] (->> input
                               str/split-lines
                               (map #(-> %
                                         (str/split #"( +)")
                                         rest)))]
    (map #(hash-map :time (parse-long %1)
                    :distance (parse-long %2))
         times
         distances)))

(defn winning-distances [{:keys [time distance]}]
  (let [possible-distances (map #(* % (- time %)) (range 0 time))
        winning-distances (filter #(< distance %) possible-distances)]
    winning-distances))

(->> input
     parse-input
     (map winning-distances)
     (map count)
     (reduce *))


;; Part 2

(defn parse-input-2 [input]
  (let [[time distance] (->> input
                             str/split-lines
                             (map #(-> %
                                       (str/split #"(: +)")
                                       second
                                       (str/replace " " "")
                                       parse-long)))]
    {:time     time
     :distance distance}))

(->> input
     parse-input-2
     winning-distances
     count)
