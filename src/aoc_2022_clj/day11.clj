(ns aoc-2022-clj.day11
  (:require [clojure.string :as str]))

(defn do-monkey-op [{:keys [operation]} val]
  (if (= (second operation) 'old)
    (* val val)
    (case (first operation)
      * (* (second operation) val)
      + (+ (second operation) val))))

(defn parse-monkey [input]
  (let [lines (str/split-lines input)]
    {:idx (read-string (second (str/split (get lines 0) #"[ :]")))
     :inventory (read-string (str "[" (second (str/split (get lines 1) #"items: ")) "]"))
     :operation (read-string (str "[" (second (str/split (get lines 2) #"new = old ")) "]"))
     :test      (read-string (last (str/split (get lines 3) #" ")))
     :if-true   (read-string (last (str/split (get lines 4) #" ")))
     :if-false  (read-string (last (str/split (get lines 5) #" ")))
     :iterations 0}))

(defn prime-products [state]
  (->> (map #(:test %) state)
       (reduce *)))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map parse-monkey)
       (vec)))

(defn throw-items [curr-monkey init-state]
  (let [init-state (assoc-in init-state [(:idx curr-monkey) :iterations]
                             (+ (:iterations curr-monkey) (count (:inventory curr-monkey))))
        init-vals (->> (:inventory curr-monkey)
                       (map #(do-monkey-op curr-monkey %))
                       (map #(mod % (prime-products init-state)))
                       (map #(vector (= (mod % (:test curr-monkey)) 0) %)))]
    (loop [vals init-vals
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
  (loop [c 10000
         state init-state]
    (if (= c 0)
      state
      (recur (- c 1) (do-round state)))))

(def start-state (parse (slurp "input/day11.txt")))
(def end-state (solve start-state))

(->> (map #(:iterations %) end-state)
     (sort)
     (take-last 2)
     (reduce *))
