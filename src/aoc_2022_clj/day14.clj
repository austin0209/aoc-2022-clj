(ns aoc-2022-clj.day14)

(defn get-pos [state x y]
  (if (or (< x 0) (>= x (:w state)) (< y 0) (>= y (:h state)))
    nil
    (get (:world state) (+ (* y (:w state)) x))))

(defn get-pos-vec [state pos]
  (get-pos state (first pos) (second pos)))
