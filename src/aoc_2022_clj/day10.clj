(ns aoc-2022-clj.day10
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer (pprint)]))

(defn parse [input]
  (->> (str/replace input "addx" "add-begin\nadd")
       (str/split-lines)
       (map #(read-string (str "[" % "]")))))

(defn simulate [{:keys [cycle x] :as state} command]
  (case (first command)
    (add-begin noop) (assoc state :cycle (+ cycle 1))
    add (-> (assoc state :cycle (+ cycle 1))
            (assoc :x (+ x (int (second command)))))))

(defn capture-states [init-state commands]
  (loop [state init-state
         commands commands
         all-states ()]
    (if (empty? commands)
      all-states
      (let [next-state (simulate state (first commands))
            updated-states (conj all-states next-state)]
        (recur next-state (rest commands) updated-states)))))

(def start-state {:cycle 0
                  :x     1})

(def commands (parse (slurp "input/day10.txt")))

(def all-states (reverse (conj (vec (capture-states start-state commands)) start-state)))

(def crt-draw (->> all-states
                   (map (fn [{:keys [cycle x]}]
                          (let [idx (mod cycle 40)]
                            (cond
                              (> (Math/abs ^int (- x idx)) 1) "."
                              :else "#"))))
                   (partition 40)))
(doseq [x crt-draw] (println x))
