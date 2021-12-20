(ns d16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn pad-to-4bits [bits]
  (let [padded-bits (str "0000" bits)]
    (.substring padded-bits (- (count padded-bits) 4))))

(defn parse [data]
  (->> data
       (map #(str/split % #""))
       flatten
       (map #(Integer/parseInt % 16))
       (map #(Integer/toBinaryString %))
       (map pad-to-4bits)
       (map seq)
       flatten))

(defn to-int [bytes]
  (Integer/parseInt (apply str bytes) 2))

(defn to-long [bytes]
  (Long/parseLong (apply str bytes) 2))

(defn parse-version [p]
  (merge p
         {:version (to-int (take 3 (:d p)))
          :b (concat (:b p) (take 3 (:d p)))
          :d (drop 3 (:d p))}))

(defn parse-type [p]
  (merge p
         {:type (to-int (take 3 (:d p)))
          :b (concat (:b p) (take 3 (:d p)))
          :d (drop 3 (:d p))}))

(defn parse-literal
  ([p]
   (let [pl (parse-literal [] (:d p))]
     (merge p
            {:literal (to-long (flatten pl))
             :b (concat (:b p) (take (* 5 (count pl)) (:d p)))
             :d (drop (take (* 5 (count pl))) (:d p))})))
  ([bits data]
   (let [to-parse (take 5 data)]
     (if (= \0 (first to-parse))
       (conj bits (next to-parse))
       (recur (conj bits (next to-parse)) (drop 5 data))))))

(defn parse-length-type [p]
  (let [ltype (first (:d p))]
    (merge p {:length-type ltype
              :counter (if (= \0 ltype) (to-int (rest (take 16 (:d p))))
                           (to-int (rest (take 12 (:d p)))))
              :b (concat (:b p) (take (if (= \0 ltype) 16 12) (:d p)))
              :d (drop (if (= \0 ltype) 16 12) (:d p))})))

(declare parse-packet)

(defn parse-subpackets
  ([p]
   (let [parsed-subp (parse-subpackets [] (:length-type p) (:counter p) (:d p))]
     (merge p
            {:subpackets parsed-subp
             :b (apply concat (:b p) (map :b parsed-subp))
             :d (drop (reduce + (map #(count (:b %)) parsed-subp)) (:d p))})))
  ([subp lt counter data]
   (if (> counter 0)
     (let [sp (parse-packet data)
           sp-length (count (:b sp))]
       (if (= \0 lt)
         (recur (conj subp sp) lt (- counter sp-length) (drop sp-length data))
         (recur (conj subp sp) lt (dec counter) (drop sp-length data))))
     subp)))

(defn parse-operator [p]
  (merge p
         (-> (parse-length-type p)
             parse-subpackets)))

(defn parse-packet [data]
  (-> {:d data}
      parse-version
      parse-type
      ((fn [p] (if (= 4 (:type p))
                 (parse-literal p)
                 (parse-operator p))))
      (dissoc :d)))

(defn parse-all-packets [data]
  (loop [parsed []
         to-parse data]
    (if (not-every? #(= \0 %) to-parse)
      (let [pp (parse-packet to-parse)]
        (recur (conj parsed pp) (drop (count (:b pp)) to-parse)))
      parsed)))

(defn sum-versions [packets]
  (loop [acc 0
         pkts packets]
    (if (empty? pkts)
      acc
      (recur (+ acc (:version (first pkts)) (sum-versions (:subpackets (first pkts)))) (next pkts)))))

(defn answer-part-1 []
  (let [d16 (parse (read-data-safe "resources/d16.txt"))]
    (->> d16
         parse-all-packets
         sum-versions)))

(defn calculate-value [packet]
  (case (:type packet)
    4 (:literal packet)
    0 (apply + (map calculate-value (:subpackets packet)))
    1 (apply * (map calculate-value (:subpackets packet)))
    2 (apply min (map calculate-value (:subpackets packet)))
    3 (apply max (map calculate-value (:subpackets packet)))
    5 (if (apply > (map calculate-value (:subpackets packet))) 1 0)
    6 (if (apply < (map calculate-value (:subpackets packet))) 1 0)
    7 (if (apply = (map calculate-value (:subpackets packet))) 1 0)))

(defn answer-part-2 []
  (let [d16 (parse (read-data-safe "resources/d16.txt"))]
    (->> d16
         parse-all-packets
         (#(calculate-value (first %))))))