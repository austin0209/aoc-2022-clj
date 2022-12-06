(ns aoc-2022-clj.day05
  (:require [clojure.string :as str]))

(defn solve [stacks commands]
  (if (empty? commands)
    (map #(take-last 1 %) stacks)
    (let [curr (first commands)
          move (first curr)
          from (second curr)
          to (get curr 2)
          from-stack (get stacks (- from 1))
          to-stack (get stacks (- to 1))
          removed (take-last move from-stack)
          new-to (apply conj to-stack removed)
          new-from (vec (take (- (count from-stack) move) from-stack))
          new-stacks (-> (assoc stacks (- from 1) new-from) (assoc (- to 1) new-to))]
      (solve new-stacks (drop 1 commands)))))

(defn parse-stacks [input]
  (vec (->> (str/split-lines input)
            (map vec))))

(defn parse-instructions [input]
  (->> (str/split-lines input)
       (map #(str "(" % ")"))
       (map #(read-string %))
       (map #(filter number? %))
       (map vec)))

(def splitted-input (-> (slurp "input/day05.txt") (#(str/split % #"\n\n"))))
(def parsed-stacks (parse-stacks (first splitted-input)))
(def parsed-instr (parse-instructions (second splitted-input)))

(print (solve parsed-stacks parsed-instr))

;(def foo `[[1 2 3] [4 5 6]])
;(def bar (get foo 0))
;(print (assoc foo 0 (apply conj bar [1 2 3])))