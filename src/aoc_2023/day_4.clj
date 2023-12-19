(ns aoc-2023.day-4
  (:require [clojure.set :as set]
            [clojure.math :as math]
            [clojure.string :as str]))

(def input (slurp "resources/inputs/day_4.txt"))

;; Part 1

(def sample-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-numbers-str [numbers-str]
  (->> (str/split numbers-str #" ")
       (remove empty?)
       (map parse-long)))

(defn parse-card [card-text]
  (let [[card-text my-numbers-str winning-numbers-str] (str/split card-text #"[:|]")]
    {:card-number     (-> card-text (str/split #"\W+") second parse-long)
     :my-numbers      (parse-numbers-str my-numbers-str)
     :winning-numbers (parse-numbers-str winning-numbers-str)}))

(defn winning-numbers [card]
  (set/intersection (-> card :my-numbers set)
                    (-> card :winning-numbers set)))

(defn card-points [card]
  (let [winning-numbers (winning-numbers card)
        matches (count winning-numbers)]
    (if (< 0 matches)
      (math/pow 2 (dec matches))
      0)))

(->> input
     str/split-lines
     (map parse-card)
     (map card-points)
     (reduce +))


;; Part 2

(defn add-bonus-cards [card]
  (let [winning-numbers (winning-numbers card)
        matches (count winning-numbers)]
    (assoc card
      :bonus-cards
      (range (inc (:card-number card))
             (inc (+ (:card-number card) matches))))))

(defn add-copies [cards]
  (loop [cards' cards
         total-bonuses {}]
    (if (empty? cards')
      (map #(assoc % :total-bonuses (get total-bonuses (:card-number %))) cards)
      (recur (rest cards')
             (assoc
               total-bonuses
               (:card-number (first cards'))
               (->> (first cards')
                    :bonus-cards
                    (map total-bonuses)
                    (apply +)
                    (+ 1)))))))

(->> input
     str/split-lines
     (map parse-card)
     (map add-bonus-cards)
     reverse
     add-copies
     (map :total-bonuses)
     (reduce +))
