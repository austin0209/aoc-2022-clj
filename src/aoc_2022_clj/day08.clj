(ns aoc-2022-clj.day08
  (:require [clojure.string :as str]))

(def map-w 99)
(def map-h 99)

(defn get-pos [state x y]
  (if (or (< x 0) (>= x map-w) (< y 0) (>= y map-h))
    nil
    (get state (+ (* y map-w) x))))

(defn parse [input]
  (->> (slurp input)
       (str/split-lines)
       (map #(seq %))
       (flatten)
       (map #(- (int %) (int \0)))
       (vec)))

(defn is-visible-dir [val pos dir trees]
  (let [next-pos (map + pos dir)
        next (apply get-pos (cons trees next-pos))]
    (cond
      (nil? next) true
      (< next val) (is-visible-dir val next-pos dir trees)
      :else false)))

(defn solve [trees]
  (->> (for [x (range map-w) y (range map-h)] [x y])
       (map #(let [v (apply get-pos (cons trees %))]
               (or (is-visible-dir v % '(0 1) trees)
                   (is-visible-dir v % '(0 -1) trees)
                   (is-visible-dir v % '(1 0) trees)
                   (is-visible-dir v % '(-1 0) trees))))
       (filter true?)
       (count)))

(def trees (parse "input/day08.txt"))
(print (solve trees))
