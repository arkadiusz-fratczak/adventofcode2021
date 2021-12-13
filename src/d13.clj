(ns d13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;--- Day 13: Transparent Origami ---
;
;You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.
;
;Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:
;
;Congratulations on your purchase! To activate this infrared thermal imaging
;camera system, please enter the code found on page 1 of the manual.
;
;Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:
;
;"6,10"
;"0,14"
;"9,10"
;"0,3"
;"10,4"
;"4,11"
;"6,0"
;"6,12"
;"4,1"
;"0,13"
;"10,12"
;"3,4"
;"3,0"
;"8,4"
;"1,10"
;"2,14"
;"8,10"
;"9,0"
;""
;"fold along y=7"
;"fold along x=5"
;
;The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:
;
;...#..#..#.
;....#......
;...........
;#..........
;...#....#.#
;...........
;...........
;...........
;...........
;...........
;.#....#.##.
;....#......
;......#...#
;#..........
;#.#........
;
;Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):
;
;...#..#..#.
;....#......
;...........
;#..........
;...#....#.#
;...........
;...........
;-----------
;...........
;...........
;.#....#.##.
;....#......
;......#...#
;#..........
;#.#........
;
;Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:
;
;#.##..#..#.
;#...#......
;......#...#
;#...#......
;.#.#..#.###
;...........
;...........
;
;Now, only 17 dots are visible.
;
;Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.
;
;Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.
;
;The second fold instruction is fold along x=5, which indicates this line:
;
;#.##.|#..#.
;#...#|.....
;.....|#...#
;#...#|.....
;.#.#.|#.###
;.....|.....
;.....|.....
;
;Because this is a vertical line, fold left:
;
;#####
;#...#
;#...#
;#...#
;#####
;.....
;.....
;
;The instructions made a square!
;
;The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.
;
;How many dots are visible after completing just the first fold instruction on your transparent paper?

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

(defn answer []
  (let [d13 (parse (read-data-safe "resources/d13.txt"))]
    (->> (fold d13)
         :points
         count)))

;--- Part Two ---
;
;Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.
;
;What code do you use to activate the infrared thermal imaging camera system?

(defn answer2 []
  (let [d13 (parse (read-data-safe "resources/d13.txt"))]
    (view
     (loop [data d13]
       (if (empty? (:folds data))
         data
         (recur (fold data)))))))

