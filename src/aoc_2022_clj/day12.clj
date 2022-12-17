(ns aoc-2022-clj.day12
  (:require [clojure.string :as str]))

(defn get-pos [state x y]
  (if (or (< x 0) (>= x (:w state)) (< y 0) (>= y (:h state)))
    nil
    (get (:world state) (+ (* y (:w state)) x))))

(defn extract-start-end [state]
  (loop [positions (for [y (range (:h state)) x (range (:w state))] [x y])
         state state]
    (if (empty? positions)
      (assoc state :world (->> (replace {\S \a, \E \z} (:world state))
                               (map #(- (int %) (int \a)))))
      (let [x (first (first positions))
            y (second (first positions))
            new-state (case (get-pos state x y)
                        \S (assoc state :start [x y])
                        \E (assoc state :end [x y])
                        state)]
        (recur (rest positions) new-state)))))

(defn parse [input]
  (let [lines (str/split-lines input)
        init-state {:w     (count (first lines))
                    :h     (count lines)
                    :world (->> lines
                                (map #(seq %))
                                (flatten)
                                (vec))}]
    (extract-start-end init-state)))

(def foo (parse (slurp "input/day12.txt")))
foo

