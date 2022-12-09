(ns aoc-2022-clj.day07
  (:require [clojure.string :as str]))

(defn get-size [f files]
  (if (number? f)
    f
    (let [contents (get files f)]
      (reduce + (map #(get-size % files) contents)))))

(defn solve [commands current-dir files history]
  (if (empty? commands)
    files
    (let [c (first commands)]
      (cond
        (number? (first c))
          (solve (rest commands)
                 current-dir
                 (assoc files current-dir (cons (first c) (get files current-dir)))
                 history)
        (= (first c) (symbol "dir"))
          (solve (rest commands)
                 current-dir
                 (assoc files current-dir (cons (str/join "/" (cons (second c) history)) (get files current-dir)))
                 history)
        (= (first c) (symbol "$"))
          (if (= (get c 2) (symbol "DOTDOT"))
            (solve (rest commands) (first history) files (rest history))
            (let [new-dir (str/join "/" (cons (get c 2) history))
                  new-history (cons (get c 2) history)]
              (solve (rest commands) new-dir files new-history)))))))

(defn parse [input]
  (->> (str/replace (str/replace input ".." "DOTDOT") " /" " ROOT")
       (str/split-lines)
       (map #(str "[" % "]"))
       (map read-string)
       (filter #(not (= % [(symbol "$") (symbol "ls")])))))

(def parsed-input (parse (slurp "input/day07.txt")))
(def files (solve (rest parsed-input) "ROOT" {"ROOT" ()} '("ROOT")))

(def unused-space (- 70000000 (get-size "ROOT" files)))

(->> (map #(get-size (first %) files) files)
     (sort)
     (filter #(>= (+ % unused-space) 30000000))
     (first)
     (print))
