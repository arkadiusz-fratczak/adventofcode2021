(ns d12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (let [d (->> data
               (map #(str/split % #"\-"))
               flatten)]
    (loop [v1 (first d)
           v2 (second d)
           rest (drop 2 d)
           res {}]
      (if (nil? v1)
        res
        (recur (first rest)
               (second rest)
               (drop 2 rest)
               (cond-> res
                 (and
                  (not= "end" v1)
                  (not= "start" v2)) (update v1 conj v2)
                 (and
                  (not= "start" v1)
                  (not= "end" v2)) (update v2 conj v1)))))))

(defn paths [cur-path next-loc graph]
  (if (= next-loc "end")
    [(conj cur-path next-loc)]
    (if (nil? (get graph next-loc))
      [(conj cur-path (str next-loc "_v"))]
      (reduce
       (fn [p nl]
         (concat p (paths
                    (conj cur-path next-loc)
                    nl
                    (cond-> graph
                      (every? #(Character/isLowerCase %) next-loc) (dissoc graph next-loc)))))
       []
       (get graph next-loc)))))

(defn answer-part-1 []
  (let [d12 (parse (read-data-safe "resources/d12.txt"))]
    (->> d12
         (paths [] "start")
         (filter (fn [v] (some #(= "end" %) v)))
         count)))

(defn adjust-graph [graph cur-path]
  (let [small-caves (->> cur-path
                         (filter #(every? (fn [c] (Character/isLowerCase c)) %))
                         frequencies)]
    (if (some #(< 1 %) (vals small-caves))
      (->> (apply dissoc graph (keys small-caves))
           (map (fn [[k v]] (hash-map k (remove #(contains? (set (keys small-caves)) %) v))))
           (apply merge))
      graph)))

(defn new-paths [cur-path next-loc graph]
  (if (= next-loc "end")
    [(conj cur-path next-loc)]
    (if (nil? (get graph next-loc))
      [(conj cur-path (str next-loc "_v"))]
      (reduce
       (fn [p nl]
         (concat p (new-paths
                    (conj cur-path next-loc)
                    nl
                    (adjust-graph graph (conj cur-path next-loc)))))
       []
       (get graph next-loc)))))

;slow
(defn answer-part-2 []
  (let [d12 (parse (read-data-safe "resources/d12.txt"))]
    (->> d12
         (new-paths [] "start")
         (filter (fn [v] (some #(= "end" %) v)))
         count)))
