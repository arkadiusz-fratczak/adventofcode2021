(ns d2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn answer-part-1 []
  (let [d2 (read-data-safe "resources/d2.txt")
        hpos (->> d2
                  (filter #(str/includes? % "forward"))
                  (map #(str/split % #" "))
                  (map (fn [[c v]] (Integer/parseInt v)))
                  (reduce +))
        vpos (->> d2
                  (filter #(not (str/includes? % "forward")))
                  (map #(str/split % #" "))
                  (map (fn [[c v]] (case c
                                     "up" (- (Integer/parseInt v))
                                     "down" (Integer/parseInt v))))
                  (reduce +))]
    (* hpos vpos)))

(defn answer-part-2 []
  (let [d2 (read-data-safe "resources/d2.txt")]
    (loop [cmd (first d2)
           fw 0
           aim 0
           dpth 0
           coll (rest d2)]
      (if cmd
        (let [[c v] (str/split cmd #" ")
              val (Integer/parseInt v)]
          (case c
            "up" (recur (first coll) fw (- aim val) dpth (rest coll))
            "down" (recur (first coll) fw (+ aim val) dpth (rest coll))
            "forward" (recur (first coll) (+ fw val) aim (+ dpth (* val aim)) (rest coll))))
        (* fw dpth)))))