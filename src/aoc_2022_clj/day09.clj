(ns aoc-2022-clj.day09
  (:require [clojure.string :as str]))

(defn move-tail [head-pos tail-pos]
  (let [x-diff (- (first head-pos) (first tail-pos))
        y-diff (- (second head-pos) (second tail-pos))
        x-dist (Math/abs ^int x-diff)
        y-dist (Math/abs ^int y-diff)]
    (if (and (<= x-dist 1) (<= y-dist 1))
      tail-pos
      (let [x-dir (Integer/signum x-diff)
            y-dir (Integer/signum y-diff)]
        (map + tail-pos (vector x-dir y-dir))))))

(defn move-all [head-pos init-knots]
  (loop [knots (assoc init-knots 0 (move-tail head-pos (get init-knots 0))) ; move the first knot next to head
         idx 1]
    (if (< idx (count knots))
      (let [lead-knot (get knots (- idx 1))
            back-knot (get knots idx)
            updated-knots (assoc knots idx (move-tail lead-knot back-knot))]
        (recur updated-knots (+ idx 1)))
      knots)))

(defn simulate [init-state [dir distance]]
  (loop [{:keys [head-pos knots-positions visited] :as state} init-state
         to-move distance]
    (if (= to-move 0)
      (assoc state :visited (conj (:visited state) (peek (:knots-positions state))))
      (let [new-head-pos (map + head-pos dir)
            new-knots-pos (move-all new-head-pos knots-positions)
            new-visited (conj visited (peek knots-positions))
            new-state (-> (assoc state :head-pos new-head-pos)
                          (assoc :knots-positions new-knots-pos)
                          (assoc :visited new-visited))]
        (recur new-state (- to-move 1))))))

(defn parse [input]
  (-> (str/replace input "R" "(1 0)")
      (str/replace "L" "(-1 0)")
      (str/replace "U" "(0 1)")
      (str/replace "D" "(0 -1)")
      (->> (str/split-lines)
           (map #(str "[" % "]"))
           (map #(read-string %)))))

(defn visualize [{:keys [visited]}]
  (let [minimum (apply min (flatten (vec visited)))
        maximum (apply max (flatten (vec visited)))
        translation (vector (Math/abs ^int minimum) (Math/abs ^int minimum))
        translated-visited (set (map #(map + translation %) (vec visited)))
        board-size (- maximum minimum -1)
        rows (for [y (range board-size)]
               (for [x (range board-size)]
                 (if (contains? translated-visited (list x (- board-size y)))
                   "#"
                   ".")))]
    (doseq [x rows] (println x))))

(def start {:head-pos '(0 0)
            :knots-positions (vec (for [_ (range 9)] '(0 0)))
            :visited #{'(0, 0)}})
(def end (reduce simulate start (parse (slurp "input/day09.txt"))))

(visualize end)

(println (count (:visited end)))
