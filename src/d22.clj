(ns d22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn read-data [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (map #(first (re-seq #"(on|off) x=(\-?\d+\.\.\-?\d+),y=(\-?\d+\.\.\-?\d+),z=(\-?\d+\.\.\-?\d+)" %)))
       (map #(hash-map :op (keyword (nth % 1))
                       :x (mapv (fn [i] (Integer/parseInt i)) (str/split (nth % 2) #"\.\."))
                       :y (mapv (fn [i] (Integer/parseInt i)) (str/split (nth % 3) #"\.\."))
                       :z (mapv (fn [i] (Integer/parseInt i)) (str/split (nth % 4) #"\.\."))))))

(defn coord-in-range [range coord]
  (>= (last range) (last coord) (first coord) (first range)))

(defn cuboid-in-range [range cuboid]
  (and (coord-in-range range (:x cuboid))
       (coord-in-range range (:y cuboid))
       (coord-in-range range (:z cuboid))))

(defn count-cuboids [data]
  (->> data
       (reduce
        (fn [acc c]
          (let [coord-set (set
                           (for [x (range (first (:x c))
                                          (inc (last (:x c))))
                                 y (range (first (:y c))
                                          (inc (last (:y c))))
                                 z (range (first (:z c))
                                          (inc (last (:z c))))]
                             [x y z]))]
            (if (= (:op c) :on)
              (set/union acc coord-set)
              (set/difference acc coord-set))))
        #{})
       count))

; naive approach
(defn answer-for-part-1 []
  (let [data (parse (read-data "resources/d22.txt"))
        filtered-data (filter #(cuboid-in-range [-50 50] %) data)]
    (time (count-cuboids filtered-data))))

(defn answer-for-part-2 []
  (let [data (parse (read-data "resources/d22.txt"))]
    (time (count-cuboids data))))