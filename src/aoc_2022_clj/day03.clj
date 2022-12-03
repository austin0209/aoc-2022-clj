(ns aoc-2022-clj.day03
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-priority [type]
  ;(Character/isUpperCase ^Character (char type)))
  (if (Character/isUpperCase ^Character (char type))
    (- (int type) (- (int \A) 27))
    (- (int type) (- (int \a) 1))))

(defn get-common [sack]
  (let [left-comp (subvec sack 0 (/ (count sack) 2)) right-comp (subvec sack (/ (count sack) 2) (count sack))]
    (->> (set/intersection (set left-comp) (set right-comp))
        (vec)
        (first))))

(defn solve [sacks]
  (->> (map get-common sacks)
       (map get-priority)
       (reduce +)))

(defn parse [input]
  (->> (str/split-lines input)
       (map vec)))

(->> (slurp "input/day03.txt")
     (parse)
     (solve)
     (print))