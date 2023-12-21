(ns aoc-2023.day-5
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_5.txt"))

;; Part 1

(def sample-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")

(defn lookup-in-range [x map-line]
  (let [[^long dest-start ^long source-start ^long range-length] map-line]
    (when (< (dec source-start) x (+ source-start range-length))
      (+ dest-start (- x source-start)))))

(defn map-lookup-fn [map-lines]
  (fn [x]
    (or (some #(lookup-in-range x %) map-lines)
        x)))

(defn parse-map-line-str [map-line-str]
  (->> (str/split map-line-str #" ")
       (map parse-long)))

(defn parse-map-str [map-str]
  (let [[map-name-str & map-line-strs] (str/split map-str #"( map:\n|\n)")
        lookup-fn (->> map-line-strs
                       (map parse-map-line-str)
                       map-lookup-fn)]
    {(keyword map-name-str) lookup-fn}))

(defn parse-seeds [seeds-str]
  (->> (str/split seeds-str #"(seeds:)| ")
       (map parse-long)
       (remove nil?)))

(defn seed->location [seed mappings]
  (-> seed
      ((:seed-to-soil mappings))
      ((:soil-to-fertilizer mappings))
      ((:fertilizer-to-water mappings))
      ((:water-to-light mappings))
      ((:light-to-temperature mappings))
      ((:temperature-to-humidity mappings))
      ((:humidity-to-location mappings))))

(let [[seeds-str & map-strs] (str/split input #"(\n\n)")
      seeds (parse-seeds seeds-str)
      mappings (apply merge (map parse-map-str map-strs))]
  (->> seeds
       (map #(seed->location % mappings))
       (reduce min)))

;; Part 2

(defn parse-seeds-range [seeds-str]
  (let [seeds-pairs (partition 2 (parse-seeds seeds-str))
        seeds-ranges (map #(range (first %)
                                  (+ (first %) (second %)))
                          seeds-pairs)
        seeds (flatten seeds-ranges)]
    seeds))

;; Too slow...
(let [[seeds-str & map-strs] (str/split input #"(\n\n)")
      seeds (parse-seeds-range seeds-str)
      mappings (apply merge (map parse-map-str map-strs))]
  (->> seeds
       (map #(seed->location % mappings))
       (reduce min)))
