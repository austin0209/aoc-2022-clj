(ns aoc-2022-clj.day01
  (:require [clojure.string :as str]))

(defn sum [groupStr]
  (->> (str/split groupStr #"\n")
       (map #(Integer/parseInt %) ,,,)
       (reduce +)))

(defn answer [input]
  (->> (str/split input #"\n\n")
       (map sum ,,,)
       (sort ,,,)
       ))

(def totals (answer (slurp "input/day01.txt")))

(println "Part 1:" (last totals))
(->> (take-last 3 totals)
     (reduce + ,,,)
     (println "Part 2:" ,,,))