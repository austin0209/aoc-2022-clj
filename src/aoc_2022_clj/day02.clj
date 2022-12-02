(ns aoc-2022-clj.day02
  (:require [clojure.string :as str]))

(def rock 1)
(def paper 2)
(def scissors 3)

(defn sanitize
  [input]
  (-> (str/replace input #"[AX]" "rock")
      (str/replace #"[BY]" "paper")
      (str/replace #"[CZ]" "scissors")))

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(str "(" % ")"))
       (map read-string)))

(defn get-outcome-score
  [round]
  (cond (= round '(rock rock)) 3
        (= round '(rock paper)) 6
        (= round '(rock scissors)) 0
        (= round '(paper rock)) 0
        (= round '(paper paper)) 3
        (= round '(paper scissors)) 6
        (= round '(scissors rock)) 6
        (= round '(scissors paper)) 0
        (= round '(scissors scissors)) 3))

(defn solve
  [rounds]
  (let [score1 (map #(eval (second %)) rounds)
        score2 (map get-outcome-score rounds)]
    (reduce + (map + score1 score2))))

(print (solve (parse (sanitize (slurp "input/day02.txt")))))