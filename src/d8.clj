(ns d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn answer-part-1 []
  (let [d8 (read-data-safe "resources/d8.txt")]
    (->> d8
         (map #(str/split % #" \| "))
         (map last)
         (map #(str/split % #" "))
         (map #(filter (fn [s] (contains? #{2 3 4 7} (count s))) %))
         flatten
         count)))

(defn parse [data]
  (->> data
       (map #(str/split % #" \| "))
       (map (fn [[samples digits]] {:samples (str/split samples #" ")
                                    :digits  (map set (str/split digits #" "))}))))

(defn find-sample
  ([samples letters-count]
   (->> samples
        (filter #(= letters-count (count %)))
        (map set)))
  ([samples letters-count must-contain]
   (->> (find-sample samples letters-count)
        (filter #(empty? (set/difference must-contain %))))))

(defn solve [{:keys [samples digits]}]
  (let [one (first (find-sample samples 2))
        seven (first (find-sample samples 3))
        four (first (find-sample samples 4))
        eight (first (find-sample samples 7))
        seg-a (set/difference seven one)
        seg-eg (set/difference eight seven four)
        seg-bd (set/difference four one)
        zero (first (find-sample samples 6 (set/union seven seg-eg)))
        seg-b (set/difference zero seven seg-eg)
        six (first (find-sample samples 6 (set/union seg-a seg-bd seg-eg)))
        seg-f (set/difference six seg-a seg-bd seg-eg)
        seg-c (set/difference one seg-f)
        nine (first (find-sample samples 6 (set/union seven four)))
        two (set/difference eight seg-b seg-f)
        three (set/difference nine seg-b)
        five (set/difference nine seg-c)
        mapping {zero  "0"
                 one   "1"
                 two   "2"
                 three "3"
                 four  "4"
                 five  "5"
                 six   "6"
                 seven "7"
                 eight "8"
                 nine  "9"}]
    (->> digits
         (map #(get mapping %))
         (apply str)
         (Integer/parseInt))))

(defn answer-part-2 []
  (let [d8 (parse (read-data-safe "resources/d8.txt"))]
    (->> d8
         (map solve)
         (reduce +))))
