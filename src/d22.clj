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

(defn naive-count-cuboids [data]
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
    (time (naive-count-cuboids filtered-data))))

(defn diff-range [[r-min r-max] [r2-min r2-max]]
  (cond
    (<= r2-min r-min r-max r2-max) []
    (and (< r-min r2-min)
         (< r2-max r-max)) [[r-min (dec r2-min)]
                            [(inc r2-max) r-max]]
    (or (< r2-max r-min)
        (< r-max r2-min)) [[r-min r-max]]
    (<= r2-min r-max r2-max) [[r-min (dec r2-min)]]
    (<= r2-min r-min r2-max) [[(inc r2-max) r-max]]))

(defn inter-range [[r-min r-max] [r2-min r2-max]]
  (cond
    (<= r2-min r-min r-max r2-max) [r-min r-max]
    (and (< r-min r2-min)
         (< r2-max r-max)) [r2-min r2-max]
    (or (< r2-max r-min)
        (< r-max r2-min)) []
    (<= r2-min r-max r2-max) [r2-min r-max]
    (<= r2-min r-min r2-max) [r-min r2-max]))

(defn diff [c1 c2]
  (let [diff-z (diff-range (:z c1) (:z c2))
        inter-z (inter-range (:z c1) (:z c2))
        diff-x (diff-range (:x c1) (:x c2))
        inter-x (inter-range (:x c1) (:x c2))
        diff-y (diff-range (:y c1) (:y c2))]
    (concat (map #(hash-map :x (:x c1)
                            :y (:y c1)
                            :z %) diff-z)
            (map #(hash-map :x %
                            :y (:y c1)
                            :z inter-z) diff-x)
            (map #(hash-map :x inter-x
                            :y %
                            :z inter-z) diff-y))))

(defn cuboid-in-cuboid [c1 c2]
  (and (not-empty (inter-range (:x c1) (:x c2)))
       (not-empty (inter-range (:y c1) (:y c2)))
       (not-empty (inter-range (:z c1) (:z c2)))))

(defn add-cuboids [list-of-cuboids c]
  (conj
   (reduce
    (fn [acc lc]
      (if (cuboid-in-cuboid lc c)
        (vec (concat acc (diff lc c)))
        (conj acc lc)))
    []
    list-of-cuboids)
   c))

(defn remove-cuboids [list-of-cuboids c]
  (reduce
   (fn [acc lc]
     (if (cuboid-in-cuboid lc c)
       (vec (concat acc (diff lc c)))
       (conj acc lc)))
   []
   list-of-cuboids))

(defn count-single-cuboids [c]
  (let [[x-min x-max] (:x c)
        [y-min y-max] (:y c)
        [z-min z-max] (:z c)]
    (* (inc (- x-max x-min))
       (inc (- y-max y-min))
       (inc (- z-max z-min)))))

(defn count-cuboids [data]
  (->> data
       (reduce
        (fn [acc c]
          (if (= (:op c) :on)
            (add-cuboids acc c)
            (remove-cuboids acc c)))
        [])
       (map count-single-cuboids)
       (apply +)))

(defn answer-for-part-2 []
  (let [data (parse (read-data "resources/d22.txt"))]
    (time (count-cuboids data))))