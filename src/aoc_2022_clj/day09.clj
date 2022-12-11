(ns aoc-2022-clj.day09
  (:require [clojure.string :as str]))

(defn simulate [init-state [dir distance]]
  (loop [{:keys [pos visited] :as state} init-state
         to-move distance
         ignore-last (:ignore-last init-state)]
    (if (= to-move 0)
      (assoc state :ignore-last true)
      (let [new-pos (map + pos dir)
            new-visited (conj visited new-pos)
            new-state (if (and (= to-move 1) ignore-last) (assoc state :pos new-pos)
                                       (-> (assoc state :pos new-pos)
                                           (assoc :visited new-visited)))]
        (recur new-state (- to-move 1) ignore-last)))))

(defn parse [input]
  (-> (str/replace input "R" "(1 0)")
      (str/replace "L" "(-1 0)")
      (str/replace "U" "(0 1)")
      (str/replace "D" "(0 -1)")
      (->> (str/split-lines)
           (map #(str "[" % "]"))
           (map #(read-string %)))))

;; TODO: ignoring last is not correct at all! sometimes you need to ignore more than the last. see lines 6 and 7
;; in the example. it's probably better to actually keep track of both head and tail position within the state.

(def start {:pos '(0 0), :visited #{'(0, 0)}, :ignore-last true})
(def end (reduce simulate start (parse (slurp "input/day09.txt"))))
(println (count (:visited end)))
(sort-by #(str (second %) (first %)) (map vec (apply vector (:visited end))))
(-> (simulate {:pos '(0 0) :visited #{'(0, 0)} :ignore-last true} ['(1 0) 3])
    (simulate ['(0 1) 2])
    (simulate ['(-1 0) 3]))
