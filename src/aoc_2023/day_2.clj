(ns aoc-2023.day-2
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_2.txt"))

;; Part 1

(def sample-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn simplify-input [s]
  (-> s
      (str/replace "Game" "")
      (str/replace " " "")
      (str/replace "red" "r")
      (str/replace "green" "g")
      (str/replace "blue" "b")))

(defn parse-set [set-str]
  (let [cubes (str/split set-str #",")]
    {:r (or (some #(when (str/ends-with? % "r") (parse-long (apply str (drop-last %)))) cubes) 0)
     :g (or (some #(when (str/ends-with? % "g") (parse-long (apply str (drop-last %)))) cubes) 0)
     :b (or (some #(when (str/ends-with? % "b") (parse-long (apply str (drop-last %)))) cubes) 0)}))

(defn parse-game [text]
  (let [[game-str & set-strs] (str/split text #"[:|;]")]
    {:game (parse-long game-str)
     :sets (map parse-set set-strs)}))

(defn valid-game? [game]
  (and (every? #(<= (:r %) 12) (:sets game))
       (every? #(<= (:g %) 13) (:sets game))
       (every? #(<= (:b %) 14) (:sets game))))

(->> input
     simplify-input
     str/split-lines
     (map parse-game)
     (filter valid-game?)
     (map :game)
     (reduce +))


;; Part 2

(defn fewest-cubes [game]
  (reduce (fn [max-dice set]
            {:r (max (:r max-dice)
                     (:r set))
             :g (max (:g max-dice)
                     (:g set))
             :b (max (:b max-dice)
                     (:b set))})
          {:r 0 :g 0 :b 0}
          (:sets game)))

(defn set-power [set]
  (* (:r set)
     (:g set)
     (:b set)))

(->> input
     simplify-input
     str/split-lines
     (map parse-game)
     (map fewest-cubes)
     (map set-power)
     (reduce +))
