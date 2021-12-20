(ns d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data [filename]
  (->> filename
       slurp
       str/split-lines))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn answer-part-1 []
  (let [data (read-data-safe "resources/d1.txt")]
    (->> data
         (map #(Integer/parseInt %))
         (partition 2 1)
         (map (fn [[x y]] (- y x)))
         (filter #(> % 0))
         count)))

(defn answer-part-2 []
  (let [data (read-data-safe "resources/d1.txt")]
    (->> data
         (map #(Integer/parseInt %))
         (partition 3 1)
         (map (fn [[x y z]] (+ x y z)))
         (partition 2 1)
         (map (fn [[x y]] (- y x)))
         (filter #(> % 0))
         count)))