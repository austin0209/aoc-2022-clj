(ns aoc-2022-clj.day03
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-priority [type]
  (if (Character/isUpperCase ^Character (char type))
    (- (int type) (- (int \A) 27))
    (- (int type) (- (int \a) 1))))

(defn get-common [group]
  (->> (map set group)
       (reduce set/intersection)
       (vec)
       (first)))

(defn solve [groups]
  (->> (map get-common groups)
       (map get-priority)
       (reduce +)))

(defn parse [input]
  (->> (str/split-lines input)
       (map vec)
       (partition 3)))

(->> (slurp "input/day03.txt")
     (parse)
     (solve)
     (print))