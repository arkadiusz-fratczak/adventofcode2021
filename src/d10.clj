(ns d10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (map #(str/split % #""))))

(def opening-braces #{"(" "[" "{" "<"})

(def closing-braces #{")" "]" "}" ">"})

(def closing-brace-for {"(" ")"
                        "[" "]"
                        "{" "}"
                        "<" ">"})
(def scoring {")" 3
              "]" 57
              "}" 1197
              ">" 25137
              ""  0})

(defn check-corrupted-line
  [line]
  (loop [brace (first line)
         to-check (rest line)
         chunk []]
    (if (nil? brace)
      ""
      (if (and (contains? opening-braces (last chunk))
               (contains? closing-braces brace))
        (if (not= brace (closing-brace-for (last chunk)))
          brace
          (recur (first to-check) (rest to-check) (vec (drop-last chunk))))
        (recur (first to-check) (rest to-check) (conj chunk brace))))))

(defn answer-part-1 []
  (let [d10 (parse (read-data-safe "resources/d10.txt"))]
    (->> d10
         (map #(check-corrupted-line %))
         (map scoring)
         (reduce +))))

(defn complete-chunk
  [chunk]
  (->> chunk
       reverse
       (map #(closing-brace-for %))))

(defn missing-braces
  [line]
  (loop [brace (first line)
         to-check (rest line)
         chunk []]
    (if (nil? brace)
      (complete-chunk chunk)
      (if (and (contains? opening-braces (last chunk))
               (contains? closing-braces brace))
        (if (not= brace (closing-brace-for (last chunk)))
          (complete-chunk chunk)
          (recur (first to-check) (rest to-check) (vec (drop-last chunk))))
        (recur (first to-check) (rest to-check) (conj chunk brace))))))

(def brace-scoring {")" 1
                    "]" 2
                    "}" 3
                    ">" 4})

(defn chunk-scoring
  [chunk]
  (->> chunk
       (map brace-scoring)
       (reduce (fn [total v] (+ v (* total 5))) 0)))

(defn final-score [scored-chunks]
  (->> scored-chunks
       sort
       (#(nth % (quot (count %) 2)))))

(defn answer-part-2 []
  (let [d10 (parse (read-data-safe "resources/d10.txt"))]
    (->> d10
         (filter #(= "" (check-corrupted-line %)))
         (map #(missing-braces %))
         (map chunk-scoring)
         final-score)))
