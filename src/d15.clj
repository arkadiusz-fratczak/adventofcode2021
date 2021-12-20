(ns d15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

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

(defn find-neighbours [idx grid-size]
  (cond-> #{}
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
       (map-indexed (fn [i _] {i (find-neighbours i size)}))
       (apply merge)))

(defn find-next-node [node-costs]
  (key (apply min-key val node-costs)))

(defn update-costs [node-costs node parent data]
  (let [current-cost (or (get node-costs node) 999999)
        parent-cost (get node-costs parent)
        new-cost (+ parent-cost (nth data node))]
    (if (< new-cost current-cost)
      (assoc node-costs node new-cost)
      node-costs)))

(defn dijkstra [start graph data]
  (let [counter (atom 1)
        start-nbrs (get graph start)
        start-dist (reduce (fn [acc n] (assoc acc n (nth data n))) {} start-nbrs)
        start-node (key (apply min-key val start-dist))]
    (loop [node-idx start-node
           node-nbrs (set/difference (get graph start-node) #{start})
           node-dist start-dist
           processed #{start}]
      (if (= node-idx (dec (count data)))
        node-dist
        (let [_ (println (swap! counter inc) "/" (count data))
              updated-costs (dissoc (reduce
                                     (fn [acc n] (update-costs acc n node-idx data))
                                     node-dist
                                     node-nbrs)
                                    node-idx)
              updated-processed (conj processed node-idx)
              next-node (find-next-node updated-costs)
              next-nbrs (set/difference (get graph next-node) updated-processed)]
          (recur next-node next-nbrs updated-costs updated-processed))))))

(defn answer-part-1 []
  (let [d15 (parse (read-data-safe "resources/d15.txt"))
        g15 (to-graph d15 100)]
    (time (get (dijkstra 0 g15 d15) 9999))))

(defn inc-grid [grid inc]
  (mapv #(if (> (+ inc %) 9) (- (+ inc %) 9) (+ inc %)) grid))

(defn enlarge [grid size factor]
  (let [en-row (flatten (for [small-row (partition size grid)]
                          (reduce
                           (fn [acc i]
                             (concat acc (inc-grid small-row i)))
                           []
                           (range factor))))
        en-grid (flatten (reduce
                          (fn [acc i]
                            (concat acc (inc-grid en-row i)))
                          []
                          (range factor)))]
    (vec en-grid)))

(defn answer-part-2 []
  (let [d15 (parse (read-data-safe "resources/d15.txt"))
        ed15 (enlarge d15 100 5)
        g15 (to-graph ed15 (* 5 100))]
    (time (get (dijkstra 0 g15 ed15) 249999))))
