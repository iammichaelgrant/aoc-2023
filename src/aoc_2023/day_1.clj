(ns aoc-2023.day-1
  (:require [clojure.string :as str]))

(def input (slurp "resources/inputs/day_1.txt"))

;; Part 1

(def sample-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")

(defn calibration-value [text]
  (let [str-chars (str/split text #"")
        first-digit (some parse-long str-chars)
        last-digit (some parse-long (reverse str-chars))]
    (+ (* first-digit 10) last-digit)))

(->> input
     str/split-lines
     (map calibration-value)
     (reduce +))


;; Part 2

(def sample-input-2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")

(defn replace-spelled-digits [s]
  (-> s
      (str/replace "one" "one1one")
      (str/replace "two" "two2two")
      (str/replace "three" "three3three")
      (str/replace "four" "four4four")
      (str/replace "five" "five5five")
      (str/replace "six" "six6six")
      (str/replace "seven" "seven7seven")
      (str/replace "eight" "eight8eight")
      (str/replace "nine" "nine9nine")))

(->> input
     str/split-lines
     (map replace-spelled-digits)
     (map calibration-value)
     (reduce +))
