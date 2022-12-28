(ns aoc-2022-clj.day16
  (:require
    [clojure.set :as set]
    [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn parse-valve [line]
  "Returns a vector of [id, {valve info}]"
  (vector (read-string (str ":" (second (str/split line #" "))))
          {:flow-rate   (read-string (second (str/split line #"[=;]")))
           :connections (->> (read-string (str "[" (second (str/split line #"valve ")) "]"))
                             (map #(read-string (str ":" %)))
                             (set))}))

(defn parse [input]
  (->> (str/replace input #"valves" "valve")
       (str/split-lines)
       (map parse-valve)
       (reduce #(assoc %1 (first %2) (second %2)) {})))

(def get-openable
  (memoize
    (fn [valves]
      (->> (keys valves)
           (filter #(> (:flow-rate (valves %)) 0))
           (vec)))))

(defn search-valve [valves start]
  "Given a starting valve, returns a collection with all connections to other valves and their cost."
  (loop [frontier (conj PersistentQueue/EMPTY [start 0])
         visited #{start}
         ret []]
    (let [curr (first (peek frontier))
          curr-depth (second (peek frontier))]
      (if (or (empty? frontier) (= (count ret) (count (get-openable valves))))
        (filter #(> (second %) 0) ret)                      ;; remove self-looping connection
        (let [neighbors (vec (:connections (valves curr)))
              unvisited (filter #(not (contains? visited %)) neighbors)
              new-frontier (apply conj (pop frontier) (map #(vector % (+ 1 curr-depth)) unvisited))
              new-visited (apply conj visited unvisited)
              new-ret (if (> (:flow-rate (valves curr)) 0)
                        (conj ret (peek frontier))
                        ret)]
          (recur new-frontier new-visited new-ret))))))

(defn simplify [valves]
  (let [relevant-valves (conj (get-openable valves) :AA)
        ret (->> relevant-valves
                 (map #(vector % (search-valve valves %)))
                 (reduce #(assoc-in %1 [(first %2) :connections] (into (hash-map) (second %2))) valves))]
    (apply dissoc ret (set/difference (set (keys ret)) (set relevant-valves)))))

(def solve
  (memoize
    (fn [valves loc time total-pressure open]
      (let [pressure-rate (reduce #(+ %1 (:flow-rate (valves %2))) 0 open)]
        (cond
          (= 0 time) total-pressure
          (= (count open) (count (get-openable valves))) (+ total-pressure (* time pressure-rate))
          :else (let [loc-info (valves loc)
                      new-total (+ total-pressure pressure-rate)]
                  (max
                    (if (and (> (:flow-rate loc-info) 0) (not (contains? open loc))) ;; try to open valve
                      (solve valves loc (- time 1) new-total (conj open loc))
                      -1)                                   ;; return -1 to ignore this entry
                    (if (empty? (:connections loc-info))
                      new-total                             ;; stuck, just return total flow rate
                      (apply max (for [v (:connections loc-info)]
                                   (let [new-time (- time (second v))
                                         new-total (+ total-pressure (* (second v) pressure-rate))]
                                     (if (< new-time 0)
                                       ;; cannot get to next pump, at this point we can calculate remaining pressure
                                       (+ total-pressure (* pressure-rate time))
                                       (solve valves (first v) new-time new-total open)))))))))))))

(def valves (simplify (parse (slurp "input/day16.txt"))))

(solve valves :AA 30 0 #{})

