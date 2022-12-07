(ns aoc-2022-clj.day0)

(defn solve [input]
  (->> (partition 4 1 input)
       (map-indexed vector)
       (filter #(= (count (set (second %))) (count (second %))))))

(print (+ 4 (first (first (solve (slurp "input/day06.txt"))))))
