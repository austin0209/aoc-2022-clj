(ns aoc-2022-clj.day07
  (:require [clojure.string :as str]))

(def files (atom {}))

(defrecord MyFile [name parent size children])

(defn get-size [file-data]
  (if (nil? (:children file-data))
    (if (= (:size file-data) 0)
      (get-size (get @files (:name file-data)))
      (:size file-data))
    (->> (map #(get-size %) (:children file-data))
         (reduce +))))

;; DISCLAIMER: this is a very ugly solution, I am just trying to solve the problem at this point...
(defn solve [commands current-dir]
  (if (empty? commands)
    nil
    (let [curr (first commands)]
      (if (= (symbol "$") (first curr))
        (if (= (get curr 2) (symbol "DOTDOT"))
          (solve (rest commands) (:parent (get @files current-dir)))
          (let [name (keyword (get curr 2))
                new-dir (MyFile. name current-dir 0 ())
                parent-data (get @files current-dir)
                pointer-file (MyFile. name current-dir 0 nil)
                updated-parent (assoc parent-data :children (cons pointer-file (:children parent-data)))]
            (swap! files assoc name new-dir)
            (swap! files assoc current-dir updated-parent)
            (solve (rest commands) name)))
        (let [dir-data (get @files current-dir)
              new-file (MyFile. :foo current-dir (first curr) nil)
              updated-data (assoc dir-data :children (cons new-file (:children dir-data)))]
          (swap! files assoc current-dir updated-data)
          (solve (rest commands) current-dir))))))

;; solution i gave up on. i think this is the more "clojure" way to do it? couldn't figure out how to deal with cd ..
;(defn solve2 [commands]
;  (if (empty? commands)
;    nil
;    (let [c (first commands)]
;      (if (= (first c) (symbol "$"))
;        (if (= (get c 2) (symbol "DOTDOT"))
;          (cons "BACK" (solve2 (rest commands)))
;          (cons (solve2 (rest commands)) ()))
;        (cons (first c) (solve2 (rest commands)))
;        ))))

(defn parse [input]
  (->> (str/replace (str/replace input ".." "DOTDOT") " /" " ROOT")
       (str/split-lines)
       (map #(str "[" % "]"))
       (map read-string)
       (filter #(not (or (= (first %) (symbol "dir")) (= % [(symbol "$") (symbol "ls")]))))))

(def parsed-input (parse (slurp "input/day07.txt")))
(def _ (solve parsed-input nil)) ;; it's probably not good practice to call a clojure method just to mutate state...
;(print (get-size (get @files :ROOT)))
(->> (vals @files)
     (map #(vector (:name %) (get-size %)))
     ;(filter #(<= % 100000))
     ;(reduce +)
     (print))
