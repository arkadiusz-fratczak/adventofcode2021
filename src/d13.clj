(ns d13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (reduce
        (fn [acc line]
          (cond-> acc
            (not-empty (re-seq #"(\d+),(\d+)" line))
            (update :points conj (mapv #(Integer/parseInt %) (rest (first (re-seq #"(\d+),(\d+)" line)))))

            (not-empty (re-seq #"fold along x=(\d+)" line))
            (update :folds conj [(Integer/parseInt (last (first (re-seq #"fold along x=(\d+)" line)))) 0])

            (not-empty (re-seq #"fold along y=(\d+)" line))
            (update :folds conj [0 (Integer/parseInt (last (first (re-seq #"fold along y=(\d+)" line))))])))
        {:points #{}
         :folds []})
       ((fn [m] (merge m {:x (inc (apply max (map first (:points m))))
                          :y (inc (apply max (map last (:points m))))})))))

(defn fold-x [points fx]
  (set
   (map
    (fn [[x y]]
      (if (< x fx)
        [x y]
        [(- (* 2 fx) x) y]))
    points)))

(defn fold-y [points fy]
  (set
   (map
    (fn [[x y]]
      (if (< y fy)
        [x y]
        [x (- (* 2 fy) y)]))
    points)))

(defn fold [data]
  (let [[fx fy] (first (:folds data))
        {:keys [x y points]} data]
    (if (= fx 0)
      {:points (fold-y points fy)
       :folds (rest (:folds data))
       :x x
       :y fy}
      {:points (fold-x points fx)
       :folds (rest (:folds data))
       :x fx
       :y y})))

(defn view [data]
  (let [points (:points data)
        gx (:x data)
        gy (:y data)]
    (doseq [y (range gy)
            x (range gx)]
      (if (some (fn [[ix iy]] (and (= x ix) (= y iy))) points)
        (print "#")
        (print " "))
      (if (= (inc x) gx)
        (println)))))

(defn answer-part-1 []
  (let [d13 (parse (read-data-safe "resources/d13.txt"))]
    (->> (fold d13)
         :points
         count)))

(defn answer-part-2 []
  (let [d13 (parse (read-data-safe "resources/d13.txt"))]
    (view
     (loop [data d13]
       (if (empty? (:folds data))
         data
         (recur (fold data)))))))

