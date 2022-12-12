(ns aoc-2022-clj.day09
  (:require [clojure.string :as str]))

(defn move-tail? [p1 p2]
  (let [a (Math/abs ^int (- (first p1) (first p2)))
        b (Math/abs ^int (- (second p1) (second p2)))]
    (or (> a 1) (> b 1))))

(defn simulate [init-state [dir distance]]
  (loop [{:keys [pos tail-pos visited] :as state} init-state
         to-move distance]
    (if (= to-move 0)
      state
      (let [new-pos (map + pos dir)
            new-tail-pos (if (move-tail? new-pos tail-pos)
                           pos
                           tail-pos)
            new-visited (conj visited new-tail-pos)
            new-state (-> (assoc state :pos new-pos)
                          (assoc :tail-pos new-tail-pos)
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

(def start {:pos '(0 0), :tail-pos '(0 0), :visited #{'(0, 0)}})
(def end (reduce simulate start (parse (slurp "input/day09.txt"))))
(println (count (:visited end)))

(sort-by #(str (second %) (first %)) (map vec (apply vector (:visited end))))

;(-> (simulate {:pos '(0 0) :tail-pos '(0 0), :visited #{'(0, 0)}} ['(1 0) 3])
;    (simulate ['(0 1) 2])
;    (simulate ['(-1 0) 3]))

