(ns aoc-2022-clj.day12
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn get-pos [state x y]
  (if (or (< x 0) (>= x (:w state)) (< y 0) (>= y (:h state)))
    nil
    (get (:world state) (+ (* y (:w state)) x))))

(defn get-pos-vec [state pos]
  (get-pos state (first pos) (second pos)))

(defn extract-start-end [state]
  (loop [positions (for [y (range (:h state)) x (range (:w state))] [x y])
         state state]
    (if (empty? positions)
      (assoc state :world (->> (replace {\S \a, \E \z} (:world state))
                               (map #(- (int %) (int \a)))
                               (vec)))
      (let [x (first (first positions))
            y (second (first positions))
            new-state (case (get-pos state x y)
                        \S (assoc state :start [x y])
                        \E (assoc state :end [x y])
                        state)]
        (recur (rest positions) new-state)))))

(defn get-zero-elevations [state]
  (loop [positions (for [y (range (:h state)) x (range (:w state))] [x y])
         result []]
    (if (empty? positions)
      result
      (if (= 0 (get-pos-vec state (first positions)))
        (recur (rest positions) (conj result (first positions)))
        (recur (rest positions) result)))))

(defn parse [input]
  (let [lines (str/split-lines input)
        init-state {:w     (count (first lines))
                    :h     (count lines)
                    :world (->> lines
                                (map #(seq %))
                                (flatten)
                                (vec))}]
    (extract-start-end init-state)))

(defn get-neighbor-positions [state position]
  (let [up (vector (first position) (+ (second position) 1))
        down (vector (first position) (- (second position) 1))
        left (vector (- (first position) 1) (second position))
        right (vector (+ (first position) 1) (second position))]
    (->> (vector up down left right)
         (filter #(some? (get-pos state (first %) (second %)))))))

(defn solve [start state]
  (loop [frontier (conj PersistentQueue/EMPTY start)
         visited #{start}
         ;; a dict of position -> parent
         history {start nil}]
    (let [curr (peek frontier)]
      (cond
        (nil? curr) {}                                      ;; route was never reached, return empty history
        (= curr (:end state)) history
        :else (let [neighbors (get-neighbor-positions state curr)
                    unvisited (filter #(not (contains? visited %)) neighbors)
                    curr-val (get-pos-vec state curr)
                    reachable (filter #(or (<= (get-pos-vec state %) curr-val)
                                           (= 1 (abs (- (get-pos-vec state %) curr-val)))) unvisited)
                    new-frontier (apply conj (pop frontier) reachable)
                    new-visited (apply conj visited reachable)
                    new-history (reduce (fn [m k] (assoc m k curr)) history reachable)]
                (recur new-frontier new-visited new-history))))))

(defn path-trace [start end history]
  (if (empty? history)
    []
    (loop [c end, path []]
      (if (= c start)
        path
        (recur (get history c) (conj path c))))))

(def state (parse (slurp "input/day12.txt")))
state

(->> (get-zero-elevations state)
     (map #(vector % (solve % state)))
     (map #(path-trace (first %) (:end state) (second %)))
     (filter #(not (empty? %)))
     (map #(count %))
     (apply min))
