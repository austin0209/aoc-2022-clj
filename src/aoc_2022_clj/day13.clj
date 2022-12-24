(ns aoc-2022-clj.day13
  (:require [clojure.string :as str]))

(declare valid?)

(defn valid-num? [left right]
  (cond
    (= left right) :skip
    (< left right) :valid
    :else :invalid))

(defn valid-coll? [left right]
  (cond
    (and (empty? left) (empty? right)) :skip
    (and (empty? left) (seq right)) :valid
    (and (seq left) (empty? right)) :invalid
    :else (let [test (valid? (first left) (first right))]
            (if (= test :skip)
              (valid? (rest left) (rest right))
              test))))

(defn valid? [left right]
  (cond
    (and (number? left) (number? right)) (valid-num? left right)
    (and (coll? left) (coll? right)) (valid-coll? left right)
    (and (number? left) (coll? right)) (valid-coll? (vector left) right)
    (and (coll? left) (number? right)) (valid-coll? left (vector right))))

(defn solve [input]
  (map #(valid? (first %) (second %)) input))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map #(map read-string %))))

(def input (parse (slurp "input/day13.txt")))

(->> (solve input)
     (map-indexed #(vector (+ 1 %1) %2))
     (filter #(= (second %) :valid))
     (map first)
     (reduce +))
