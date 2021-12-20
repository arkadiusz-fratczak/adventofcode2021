(ns d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn answer-part-1 []
  (let [d3 (read-data-safe "resources/d3.txt")
        gamma-rate (->> d3
                        (apply map vector)
                        (map frequencies)
                        (map #(key (apply max-key val %)))
                        (apply str)
                        (#(Integer/parseInt % 2)))
        epsilon-rate (->> d3
                          (apply map vector)
                          (map frequencies)
                          (map #(key (apply min-key val %)))
                          (apply str)
                          (#(Integer/parseInt % 2)))]
    (* gamma-rate epsilon-rate)))

(defn bit-frequencies [data col]
  (->> data
       (apply map vector)
       (#(nth % col))
       frequencies))

(defn max-bit-frequencies [data col]
  (let [{bit0 \0
         bit1 \1
         :or {bit0 0
              bit1 0}} (bit-frequencies data col)]
    (cond
      (= bit1 bit0) \1
      (> bit1 bit0) \1
      (< bit1 bit0) \0)))

(defn min-bit-frequencies [data col]
  (let [{bit0 \0
         bit1 \1
         :or {bit0 0
              bit1 0}} (bit-frequencies data col)]
    (cond
      (= bit1 bit0) \0
      (> bit1 bit0) \0
      (< bit1 bit0) \1)))

(defn advanced-rating [data bit-freq-fn]
  (loop [filtered-data data
         bits ""
         col -1]
    (if (empty? (rest filtered-data))
      (first filtered-data)
      (let [new-col (inc col)
            new-bits (str bits (bit-freq-fn filtered-data new-col))
            new-data (filter #(str/starts-with? % new-bits) filtered-data)]
        (recur new-data new-bits new-col)))))

(defn oxygen-generator-rating [data]
  (advanced-rating data max-bit-frequencies))

(defn co2-scrubber-rating [data]
  (advanced-rating data min-bit-frequencies))

(defn answer-part-2
  []
  (let [d3 (read-data-safe "resources/d3.txt")
        oxygen-rating (Integer/parseInt (oxygen-generator-rating d3) 2)
        co2-rating (Integer/parseInt (co2-scrubber-rating d3) 2)]
    (* oxygen-rating co2-rating)))
