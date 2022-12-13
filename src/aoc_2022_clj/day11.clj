(ns aoc-2022-clj.day11
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer (pprint)]))

(defn do-monkey-op [{:keys [operation]} val]
  (let [op (str/replace operation #"old" (str val "N"))]
    (eval (read-string op))))

(defn parse-monkey [input]
  (let [lines (str/split-lines input)]
    {:idx (read-string (second (str/split (get lines 0) #"[ :]")))
     :inventory (read-string (str "[" (second (str/split (get lines 1) #"items: ")) "]"))
     :operation (str "(" (second (str/split (get lines 2) #"new = old ")) " old)")
     :test      (read-string (last (str/split (get lines 3) #" ")))
     :if-true   (read-string (last (str/split (get lines 4) #" ")))
     :if-false  (read-string (last (str/split (get lines 5) #" ")))
     :iterations 0}))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map parse-monkey)
       (vec)))

(defn throw-items [curr-monkey init-state]
  (let [init-state (assoc-in init-state [(:idx curr-monkey) :iterations]
                             (+ (:iterations curr-monkey) (count (:inventory curr-monkey))))
        vals (->> (:inventory curr-monkey)
                  (map #(do-monkey-op curr-monkey %))
                  (map #(int (Math/floor (float (/ % 3)))))
                  (map #(vector (= (mod % (:test curr-monkey)) 0) %)))]
    (loop [vals vals
           state init-state]
    (if (empty? vals)
      state
      (let [curr-val (first vals)
            next-monkey-idx (if (first curr-val)
                              (:if-true curr-monkey)
                              (:if-false curr-monkey))
            next-monkey (get state next-monkey-idx)
            item (second curr-val)
            new-inventory (conj (:inventory next-monkey) item)
            new-state (-> (assoc-in state [next-monkey-idx :inventory] new-inventory)
                          (assoc-in [(:idx curr-monkey) :inventory] []))]
        (recur (rest vals) new-state))))))

(defn do-round [init-state]
  (loop [state init-state
         idx 0]
    (if (>= idx (count state))
      state
      (recur (throw-items (get state idx) state) (+ idx 1)))))

(defn solve [init-state]
  (loop [c 20
         state init-state]
    (if (= c 0)
      state
      (recur (- c 1) (do-round state)))))

(def start-state (parse (slurp "input/day11.txt")))
(def end-state (solve start-state))
;(pprint end-state)

(pprint (->> (map #(:iterations %) end-state)
     (sort)
     (take-last 2)
     (reduce *)))

