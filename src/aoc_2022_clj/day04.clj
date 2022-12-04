(ns aoc_2022_clj.day04
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(defn expand-range [raw-spec] ; input ex: "2-4"
  (let [spec (read-string (str "[" (str/replace raw-spec "-" " ") "]"))
        lower (get spec 0)
        upper (+ 1 (get spec 1))]
    (set (range lower upper))))

(defn range-overlaps? [range1 range2]
  (let [r1 (expand-range range1) r2 (expand-range range2) intersect (set/intersection r1 r2)]
    (boolean (seq intersect))))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(str/split % #","))
       (map #(apply range-overlaps? %))
       (filter true?)
       (count)))

(->> (slurp "input/day04.txt")
     (parse)
     (print))
