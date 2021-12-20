(ns d11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (map #(str/split % #""))
       flatten
       (mapv #(Integer/parseInt %))))

(defn inc-energy [grid idx]
  (if (not= 0 (nth grid idx))
    (update grid idx inc)
    grid))

(defn flash-single [idx grid size]
  (cond-> (vec grid)
    ;upper left
    (and (> (rem idx size) 0)
         (>= (- idx size) 0)) (inc-energy (dec (- idx size)))
    ;upper
    (>= (- idx size) 0) (inc-energy (- idx size))
    ;upper right
    (and (< (rem idx size) (dec size))
         (>= (- idx size) 0)) (inc-energy (inc (- idx size)))
    ;left
    (> (rem idx size) 0) (inc-energy (dec idx))
    ;center
    true (assoc idx 0)
    ;right
    (< (rem idx size) (dec size)) (inc-energy (inc idx))
    ;bottom left
    (and (> (rem idx size) 0)
         (< (+ idx size) (* size size))) (inc-energy (dec (+ idx size)))
    ;bottom
    (< (+ idx size) (* size size)) (inc-energy (+ idx size))
    ;bottom right
    (and (< (rem idx size) (dec size))
         (< (+ idx size) (* size size))) (inc-energy (inc (+ idx size)))))

(defn flash [grid size]
  (loop [o (first grid)
         r (rest grid)
         g grid]
    (if (nil? o)
      (if (every? #(< % 10) g)
        g
        (recur (first g) (rest g) g))
      (if (< o 10)
        (recur (first r) (rest r) g)
        (recur (first r) (rest r) (flash-single (- (count g) (count r) 1) g size))))))

(defn step [grid size]
  (let [inc-energy-grid (mapv inc grid)]
    (flash inc-energy-grid size)))

(defn count-flashes [grid]
  (->> grid
       (filter #(= 0 %))
       count))

(defn steps [grid steps size]
  (reduce
   (fn [acc _]
     (let [new-grid (step (:g acc) size)]
       (merge acc
              {:g new-grid
               :f (+ (:f acc) (count-flashes new-grid))})))
   {:g grid
    :f 0}
   (repeat steps 0)))

(defn view [grid]
  (update grid :g #(partition 10 %)))

(defn answer-part-1 []
  (let [d11 (parse (read-data-safe "resources/d11.txt"))]
    (-> d11
        (steps 100 10)
        view)))

(defn all-flashes [grid size]
  (loop [s 0
         g grid]
    (if (every? #(= 0 %) g)
      {:g g
       :s s}
      (recur (inc s) (step g size)))))

(defn answer-part-2 []
  (let [d11 (parse (read-data-safe "resources/d11.txt"))]
    (-> d11
        (all-flashes 10)
        view)))