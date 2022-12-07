(ns aoc-2022-clj.day06)

(defn solve [input]
  (->> (partition 14 1 input)
       (map-indexed vector)
       (filter #(= (count (set (second %))) (count (second %))))))

(print (+ 14 (first (first (solve (slurp "input/day06.txt"))))))
