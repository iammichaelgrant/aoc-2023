(ns aoc-2023.day-8
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_8.txt"))

;; Part 1

(def sample-input "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)")
(def sample-input-2 "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)")

(defn parse-node-str [node-str]
  (let [[node left right] (str/split node-str #"( = \(|, |\))")]
    {node {\L left
           \R right}}))

(let [input input
      [starting-path & node-strs] (str/split-lines input)
      nodes (map parse-node-str node-strs)
      node-map (apply merge nodes)]
  (loop [steps 0
         path starting-path
         node "AAA"]
    (if (or (= "ZZZ" node)
            (nil? node))
      steps
      (recur (inc steps)
             (if (seq (rest path))
               (rest path)
               starting-path)
             (get (get node-map node) (first path))))))
