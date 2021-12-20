(ns d4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse-drawn-numbers [line]
  (map #(Integer/parseInt %) (str/split line #",")))

(defn parse-board [numbers]
  (->> numbers
       (filter not-empty)
       (map #(Integer/parseInt %))
       ((fn [nbrs]
          (let [all (set nbrs)
                rows (partition 5 nbrs)
                cols (apply map vector rows)]
            {:all all
             :rows rows
             :cols cols
             :rows-and-cols (concat (map set rows) (map set cols))})))))

(defn parse-boards [data]
  (->> data
       (partition 6)
       (map #(str/join " " %))
       (map #(str/split % #"\s+"))
       (map parse-board)))

(defn is-winning-board [board draws]
  (->> (:rows-and-cols board)
       (some #(empty? (set/difference % (set draws))))))

(defn winning-board [boards draws]
  (first (filter #(is-winning-board % draws) boards)))

(defn answer-part-1 []
  (let [data (read-data-safe "resources/d4.txt")
        drawn-numbers (parse-drawn-numbers (first data))
        boards (parse-boards (rest data))]
    (loop [draws [(first drawn-numbers)]
           next-draws (rest drawn-numbers)]
      (if-let [wining-board (winning-board boards draws)]
        (* (last draws) (reduce + (set/difference (:all wining-board) (set draws))))
        (recur (conj draws (first next-draws)) (rest next-draws))))))

(defn last-winning-board [boards draws]
  (if (every? #(is-winning-board % draws) boards)
    (first (remove #(is-winning-board % (drop-last draws)) boards))))

(defn answer-part-2 []
  (let [data (read-data-safe "resources/d4.txt")
        drawn-numbers (parse-drawn-numbers (first data))
        boards (parse-boards (rest data))]
    (loop [draws [(first drawn-numbers)]
           next-draws (rest drawn-numbers)]
      (if-let [last-wining-board (last-winning-board boards draws)]
        (* (last draws) (reduce + (set/difference (:all last-wining-board) (set draws))))
        (recur (conj draws (first next-draws)) (rest next-draws))))))