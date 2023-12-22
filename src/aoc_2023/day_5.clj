(ns aoc-2023.day-5
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_5.txt"))

;; Part 1

(def sample-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")

(defn lookup-in-range [map-line]
  (let [[^long dest-start ^long source-start ^long range-length] map-line
        range-bot (dec source-start)
        range-top (+ source-start range-length)]
    (fn lookup [^long x]
      (when
        (and (< range-bot x)
             (< x range-top))
        (+ dest-start (- x source-start))))))

(defn map-lookup-fn [map-lines]
  (let [lookup-in-range-fns (mapv lookup-in-range map-lines)]
    (fn lookup [x]
      (loop [fns lookup-in-range-fns]
        (if (empty? fns)
          x
          (or ((first fns) x)
              (recur (next fns))))))))

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

(defn seed->location-fn [mappings]
  (comp
    (:humidity-to-location mappings)
    (:temperature-to-humidity mappings)
    (:light-to-temperature mappings)
    (:water-to-light mappings)
    (:fertilizer-to-water mappings)
    (:soil-to-fertilizer mappings)
    (:seed-to-soil mappings)))

(let [[seeds-str & map-strs] (str/split sample-input #"(\n\n)")
      seeds (parse-seeds seeds-str)
      seed->location (seed->location-fn (apply merge (map parse-map-str map-strs)))]
  (->> seeds
       (map seed->location)
       (reduce min)))

;; Part 2

(defn parse-seeds-range [seeds-str]
  (let [seeds-pairs (partition 2 (parse-seeds seeds-str))
        seeds-ranges (map #(range (first %)
                                  ;(+ (first %) (second %))
                                  (+ (first %) (/ (second %) 100))
                                  )
                          seeds-pairs)
        seeds (flatten seeds-ranges)]
    seeds))

;https://stackoverflow.com/a/19972453
(defn chunked-pmap [f partition-size coll]
  (->> coll
       (partition-all partition-size)
       (pmap (comp doall
                   (partial map f)))
       (apply concat)))

;; This is still quite slow...
(let [[seeds-str & map-strs] (str/split input #"(\n\n)")
      seeds (parse-seeds-range seeds-str)
      seed->location (seed->location-fn (apply merge (map parse-map-str map-strs)))]
  (->> seeds
       (chunked-pmap seed->location 64)
       (reduce min)))
