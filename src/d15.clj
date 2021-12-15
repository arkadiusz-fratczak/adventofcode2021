(ns d15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;--- Day 15: Chiton ---
;
;You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.
;
;The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:
;
;"1163751742"
;"1381373672"
;"2136511328"
;"3694931569"
;"7463417111"
;"1319128137"
;"1359912421"
;"3125421639"
;"1293138521"
;"2311944581"
;
;You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).
;
;Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:
;
;1163751742
;1381373672
;2136511328
;3694931569
;7463417111
;1319128137
;1359912421
;3125421639
;1293138521
;2311944581
;
;The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).
;
;What is the lowest total risk of any path from the top left to the bottom right?

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

(defn find-neighbours [idx data grid-size]
  (cond-> []
          (>= (- idx grid-size) 0)
          (conj (- idx grid-size))

          (< (+ idx grid-size) (* grid-size grid-size))
          (conj (+ idx grid-size))

          (> (rem idx grid-size) 0)
          (conj (dec idx))

          (< (rem idx grid-size) (dec grid-size))
          (conj (inc idx))))

(defn to-graph [data size]
  (->> data
       (map-indexed (fn [i _] {i (find-neighbours i data size)}))
       (apply merge)))

(defn find-next-node [node-costs processed]
  (first
    (first
      (filter #(nil? (get processed (key %)))
              (sort-by #(-> % val :c) node-costs)))))

(defn update-costs [node-costs node parent data]
  (let [current-cost (or (:c (get node-costs node)) ##Inf)
        parent-cost (:c (get node-costs parent))
        new-cost (+ parent-cost (nth data node))]
    (if (< new-cost current-cost)
      (assoc node-costs node {:p parent
                              :c new-cost})
      node-costs)))

(defn dijkstra [start graph data]
  (let [start-nbrs (get graph start)
        start-costs (reduce (fn [acc n] (assoc acc n {:p start
                                                      :c (nth data n)})) {} start-nbrs)
        start-node (key (apply min-key #(-> % val :c) start-costs))]
    (loop [node-idx start-node
           node-nbrs (get graph node-idx)
           node-costs start-costs
           processed #{}]
      (if (= node-idx (dec (count data)))
        node-costs
        (let [updated-costs (reduce
                              (fn [acc n] (update-costs acc n node-idx data))
                              node-costs
                              node-nbrs)
              updated-processed (conj processed node-idx)
              next-node (time (find-next-node updated-costs updated-processed))
              next-nbrs (get graph next-node)]
          (recur next-node next-nbrs updated-costs updated-processed))))))

; slow ~44s :(
(defn answer []
  (let [d15 (parse (read-data-safe "resources/d15.txt"))
        g15 (to-graph d15 100)]
    (time (get (dijkstra 0 g15 d15) 9999))))
