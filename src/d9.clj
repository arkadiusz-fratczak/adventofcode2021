(ns d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;--- Day 9: Smoke Basin ---
;
;These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.
;
;If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).
;
;Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:
;
;2199943210
;3987894921
;9856789892
;8767896789
;9899965678
;
;Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.
;
;Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)
;
;In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.
;
;The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.
;
;Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (map #(str/split % #""))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       flatten
       vec))

(defn find-adjacent-locations [idx data width height]
  (cond-> {:loc [idx (nth data idx)]
           :adj #{}}
    (>= (- idx width) 0) (update-in [:adj] conj [(- idx width) (nth data (- idx width))])
    (< (+ idx width) (* width height)) (update-in [:adj] conj [(+ idx width) (nth data (+ idx width))])
    (> (rem idx width) 0) (update-in [:adj] conj [(dec idx) (nth data (dec idx))])
    (< (rem idx width) (dec width)) (update-in [:adj] conj [(inc idx) (nth data (inc idx))])))

(defn find-low-points [adjacent-locations]
  (filter (fn [{:keys [loc adj]}]
            (and (= (last loc) (apply min (cons (last loc) (map last adj))))
                 (not-any? #(= (last loc) %) (map last adj))))
          adjacent-locations))

(defn answer []
  (let [d9 (parse (read-data-safe "resources/d9.txt"))]
    (->> d9
         (map-indexed (fn [i _] (find-adjacent-locations i d9 100 100)))
         find-low-points
         (map #(inc (-> % :loc last)))
         (reduce +))))

;--- Part Two ---
;
;Next, you need to find the largest basins so you know what areas are most important to avoid.
;
;A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.
;
;The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.
;
;The top-left basin, size 3:
;
;2199943210
;3987894921
;9856789892
;8767896789
;9899965678
;
;The top-right basin, size 9:
;
;2199943210
;3987894921
;9856789892
;8767896789
;9899965678
;
;The middle basin, size 14:
;
;2199943210
;3987894921
;9856789892
;8767896789
;9899965678
;
;The bottom-right basin, size 9:
;
;2199943210
;3987894921
;9856789892
;8767896789
;9899965678
;
;Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.
;
;What do you get if you multiply together the sizes of the three largest basins?

(defn find-basin
  ([idx data width height]
   (find-basin idx data width height #{}))
  ([idx data width height acc]
   (if (or (= (nth data idx) 9) (contains? acc idx))
     acc
     (let [new-acc (conj acc idx)]
       (cond->> new-acc
         (>= (- idx width) 0)
         (find-basin (- idx width) data width height)

         (< (+ idx width) (* width height))
         (find-basin (+ idx width) data width height)

         (> (rem idx width) 0)
         (find-basin (dec idx) data width height)

         (< (rem idx width) (dec width))
         (find-basin (inc idx) data width height))))))

(defn answer2 []
  (let [d9 (parse (read-data-safe "resources/d9.txt"))]
    (->> d9
         (map-indexed (fn [i _] (find-adjacent-locations i d9 100 100)))
         find-low-points
         (map #(find-basin (-> % :loc first) d9 100 100))
         (map count)
         (sort >)
         (take 3)
         (apply *))))