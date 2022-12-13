(ns aoc-2022-clj.day10
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer (pprint)]))

(defn simulate [{:keys [cycle x] :as state} command]
  (let [next-state (case (first command)
                     (add-begin noop) (assoc state :cycle (+ cycle 1))
                     add (-> (assoc state :cycle (+ cycle 1))
                             (assoc :x (+ x (int (second command))))))]
    (if (contains? #{20 60 100 140 180 220} (:cycle next-state))
      (do (pprint next-state) (assoc next-state :ans (cons (* (:cycle next-state) (:x state)) (:ans state))))
      next-state)))

(defn parse [input]
  (->> (str/replace input "addx" "add-begin\nadd")
       (str/split-lines)
       (map #(read-string (str "[" % "]")))))

(def start-state {:cycle 0
                  :x     1
                  :ans   ()})

(def end-state (reduce simulate start-state (parse (slurp "input/day10.txt"))))
(map-indexed (fn [idx item] [(+ 1 idx) item]) (parse (slurp "input/day10.txt")))

(pprint (reduce + (:ans end-state)))
