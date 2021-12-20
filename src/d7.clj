(ns d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse-data [data]
  (->> data
       first
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(defn median [sorted-data]
  (let [count (count sorted-data)
        middle (quot count 2)]
    (if (= 1 (rem count 2))
      (nth sorted-data middle)
      (nth sorted-data (/ (+ middle (dec middle)) 2)))))

(defn answer-part-1 []
  (let [d7 (parse-data
            (read-data-safe "resources/d7.txt"))
        d7sorted (sort d7)
        median (median d7sorted)]
    (->> d7
         (map #(Math/abs (- median %)))
         (reduce +))))

(defn avg-floor [data]
  (int (Math/floor (/ (reduce + data) (count data)))))

(defn avg-ceil [data]
  (int (Math/ceil (/ (reduce + data) (count data)))))

(defn cost [dist]
  (reduce + (range (inc dist))))

(defn overall-cost
  ([data]
   (min (overall-cost data avg-ceil) (overall-cost data avg-floor)))
  ([data avg-fn]
   (let [avg (avg-fn data)]
     (->> data
          (map #(Math/abs (- avg %)))
          (map cost)
          (reduce +)))))

(defn answer-part-2 []
  (let [d7 (parse-data
            (read-data-safe "resources/d7.txt"))]
    (overall-cost d7)))