(ns d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse-data [data]
  (->> data
       first
       (#(str/split % #","))
       (mapv #(Integer/parseInt %))))

(defn after-day [fishes]
  (->> fishes
       (mapv #(if (= % 0)
                [6 8]
                (dec %)))
       flatten))

(defn after-days [fishes days]
  (if (= days 0)
    fishes
    (recur (after-day fishes) (dec days))))

(defn answer-part-1 []
  (let [d6 (read-data-safe "resources/d6.txt")
        fishes (parse-data d6)]
    (count (after-days fishes 80))))

(defn count-spawned
  [days pre-computed]
  (or (get pre-computed days)
      (if (> days 0)
        (+ 1 (count-spawned (- days 7) pre-computed) (count-spawned (- days 9) pre-computed))
        0)))

(defn pre-compute-for-0 [days]
  (reduce
   (fn [acc day]
     (assoc acc day (count-spawned day acc)))
   {}
   (range days)))

(defn count-shoal [fishes days pre-computed]
  (+ (count fishes) (reduce +' (map #(count-spawned (- days %) pre-computed) fishes))))

(defn answer-part-2 []
  (let [d6 (read-data-safe "resources/d6.txt")
        fishes (parse-data d6)
        pre-computed (pre-compute-for-0 256)]
    (count-shoal fishes 256 pre-computed)))
