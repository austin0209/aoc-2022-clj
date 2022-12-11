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

(defn view-distance [val pos dir trees]
  (let [next-pos (map + pos dir)
        next (apply get-pos (cons trees next-pos))]
    (cond
      (nil? next) 0
      (< next val) (+ 1 (view-distance val next-pos dir trees))
      :else 1)))

(defn solve [trees]
  (->> (for [x (range map-w) y (range map-h)] [x y])
       (filter #(and (> (first %) 0) (< (first %) map-w)
                     (> (second %) 0) (< (second %) map-h)))
       (map #(let [v (apply get-pos (cons trees %))]
               (* (view-distance v % '(0 1) trees)
                  (view-distance v % '(0 -1) trees)
                  (view-distance v % '(1 0) trees)
                  (view-distance v % '(-1 0) trees))))
       (apply max)))

(def trees (parse "input/day08.txt"))
(print (solve trees))
