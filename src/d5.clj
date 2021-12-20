(ns d5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse-line [line]
  (let [coords (map #(Integer/parseInt %) line)
        [x1 y1 x2 y2] coords]
    (cond
      (and (= x1 x2) (= y1 y2)) {}
      (= x1 x2) {:horizontal false
                 :vertical   true
                 :formula    [0 x1]
                 :start (if (> y1 y2) [x2 y2] [x1 y1])
                 :end (if (> y1 y2) [x1 y1] [x2 y2])}
      (= y1 y2) {:horizontal true
                 :vertical   false
                 :formula    [0 y1]
                 :start (if (> x1 x2) [x2 y2] [x1 y1])
                 :end (if (> x1 x2) [x1 y1] [x2 y2])}
      :else {:horizontal false
             :vertical   false
             :formula    [(/ (- x2 x1) (- y2 y1))
                          (- y1 (* (/ (- x2 x1) (- y2 y1)) x1))]
             :start (if (> x1 x2) [x2 y2] [x1 y1])
             :end (if (> x1 x2) [x1 y1] [x2 y2])})))

(defn parse-lines [data]
  (->> data
       (map #(str/split % #",| -> "))
       (map #(parse-line %))))

(defn update-board [board line]
  (cond
    (:vertical line) (loop [b board
                            y (last (:start line))]
                       (if (> y (last (:end line)))
                         b
                         (recur (update b (+ (* 1000 (first (:start line))) y) inc) (inc y))))
    :else (loop [b board
                 x (first (:start line))]
            (if (> x (first (:end line)))
              b
              (recur (update b (+ (* 1000 x) (+ (* (first (:formula line)) x) (last (:formula line)))) inc) (inc x))))))

(defn answer-part-1 []
  (let [d5 (read-data-safe "resources/d5.txt")
        board (vec (repeat (* 1000 1000) 0))
        lines (parse-lines d5)
        vertical-or-horizontal (filter #(or (:vertical %) (:horizontal %)) lines)
        board-with-lines (reduce update-board
                                 board
                                 vertical-or-horizontal)]
    (count (filter #(>= % 2) board-with-lines))))

(defn answer-part-2 []
  (let [d5 (read-data-safe "resources/d5.txt")
        board (vec (repeat (* 1000 1000) 0))
        lines (parse-lines d5)
        board-with-lines (reduce update-board
                                 board
                                 lines)]
    (count (filter #(>= % 2) board-with-lines))))