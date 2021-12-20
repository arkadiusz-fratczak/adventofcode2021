(ns d9
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

(defn answer-part-1 []
  (let [d9 (parse (read-data-safe "resources/d9.txt"))]
    (->> d9
         (map-indexed (fn [i _] (find-adjacent-locations i d9 100 100)))
         find-low-points
         (map #(inc (-> % :loc last)))
         (reduce +))))

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

(defn answer-part-2 []
  (let [d9 (parse (read-data-safe "resources/d9.txt"))]
    (->> d9
         (map-indexed (fn [i _] (find-adjacent-locations i d9 100 100)))
         find-low-points
         (map #(find-basin (-> % :loc first) d9 100 100))
         (map count)
         (sort >)
         (take 3)
         (apply *))))