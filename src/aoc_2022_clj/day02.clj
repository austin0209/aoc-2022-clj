(ns aoc-2022-clj.day02
  (:require [clojure.string :as str]))

(def rock 0)
(def paper 3)
(def scissors 6)

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

(defn get-choice-score
  [round]
  (cond (= round '(rock rock)) 3
        (= round '(rock paper)) 1
        (= round '(rock scissors)) 2
        (= round '(paper rock)) 1
        (= round '(paper paper)) 2
        (= round '(paper scissors)) 3
        (= round '(scissors rock)) 2
        (= round '(scissors paper)) 3
        (= round '(scissors scissors)) 1))

(defn solve
  [rounds]
  (let [score1 (map #(eval (second %)) rounds)
        score2 (map get-choice-score rounds)]
    (reduce + (map + score1 score2))))

(print (solve (parse (sanitize (slurp "input/day02.txt")))))