(ns d20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (let [fl (first data)
        _ (second data)
        im (drop 2 data)]
    {:iea  (vec (str/split fl #""))
     :img  (->> im
                (map #(str/split % #"")))
     :inf  "."
     :size 100}))

(defn enhance-img [{:keys [img inf size] :as img-data}]
  (let [new-size (+ 4 size)
        new-img (concat (apply concat (repeat new-size inf)
                               (repeat new-size inf)
                               (map #(concat [inf inf] % [inf inf]) img))
                        (repeat new-size inf)
                        (repeat new-size inf))]
    (merge img-data {:size new-size
                     :img  new-img})))

(defn decode-idx [idx inf img size]
  (Integer/parseInt
   (apply str
          (map
           #(case %
              "." "0"
              "#" "1")
           (-> []
                 ;upper left
               (#(conj % (if (and (> (rem idx size) 0) (>= (- idx size) 0))
                           (nth img (dec (- idx size)))
                           inf)))
                 ;upper
               (#(conj % (if (>= (- idx size) 0)
                           (nth img (- idx size))
                           inf)))
                 ;upper right
               (#(conj % (if (and (< (rem idx size) (dec size)) (>= (- idx size) 0))
                           (nth img (inc (- idx size)))
                           inf)))
                 ;left
               (#(conj % (if (> (rem idx size) 0)
                           (nth img (dec idx))
                           inf)))
                 ;center
               (#(conj % (nth img idx)))
                 ;right
               (#(conj % (if (< (rem idx size) (dec size))
                           (nth img (inc idx))
                           inf)))
                 ;bottom left
               (#(conj % (if (and (> (rem idx size) 0) (< (+ idx size) (* size size)))
                           (nth img (dec (+ idx size)))
                           inf)))
                 ;bottom
               (#(conj % (if (< (+ idx size) (* size size))
                           (nth img (+ idx size))
                           inf)))
                 ;bottom right
               (#(conj % (if (and (< (rem idx size) (dec size)) (< (+ idx size) (* size size)))
                           (nth img (inc (+ idx size)))
                           inf))))))
   2))

(defn new-pixel [pixel-idx iea inf img size]
  (let [new-pixel-idx (decode-idx pixel-idx inf img size)
        new-pixel (nth iea new-pixel-idx)]
    new-pixel))

(defn step [{:keys [iea inf] :as img-data}]
  (let [enh-img (enhance-img img-data)
        new-size (:size enh-img)
        raw-img-flatten (vec (flatten (:img enh-img)))
        new-raw-img (map-indexed (fn [i _] (new-pixel i iea inf raw-img-flatten new-size))
                                 raw-img-flatten)
        new-inf (nth iea (Integer/parseInt (apply str (repeat 9 (case inf
                                                                  "." "0"
                                                                  "#" "1"))) 2))]
    {:iea  iea
     :img  (partition new-size new-raw-img)
     :inf  new-inf
     :size new-size}))

(defn answer []
  (let [d20 (parse (read-data-safe "resources/d20.txt"))]
    (->> d20
         (#(nth (iterate step %) 2))
         :img
         flatten
         frequencies)))

(defn answer2 []
  (let [d20 (parse (read-data-safe "resources/d20.txt"))]
    (->> d20
         (#(nth (iterate step %) 50))
         :img
         flatten
         frequencies)))