(ns d24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse-operand [op]
  (case op
    ("w" "x" "y" "z") (keyword op)
    (Integer/parseInt op)))

(defn parse-instr [[op & args]]
  (if (= op "inp")
    [:inp [(keyword (first args))]]
    [(keyword op) [(parse-operand (first args)) (parse-operand (last args))]]))

(defn parse [data]
  (->> data
       (map #(str/split % #" "))
       (map parse-instr)))

(defn inp [args acc]
  (let [data (:data acc)]
    (assoc acc (first args) (first data)
               :data (rest data))))

(defn add [[a b] acc]
  (update acc a + (or (get acc b) b)))

(defn mul [[a b] acc]
  (update acc a * (or (get acc b) b)))

(defn div [[a b] acc]
  (update acc a quot (or (get acc b) b)))

(defn modulo [[a b] acc]
  (update acc a mod (or (get acc b) b)))

(defn eql [[a b] acc]
  (assoc acc a
             (if (= (get acc a) (or (get acc b) b))
               1
               0)))

(defn alu [ins data]
  (reduce
    (fn [acc in]
      (case (first in)
        :inp (inp (last in) acc)
        :add (add (last in) acc)
        :mul (mul (last in) acc)
        :div (div (last in) acc)
        :mod (modulo (last in) acc)
        :eql (eql (last in) acc)))
    {:w 0
     :x 0
     :y 0
     :z 0
     :data data}
    ins))

(defn model-number-vec [nr]
  (->> nr
       str
       (map #(Integer/parseInt (str %)))))

(def model-numbers
  (->> (range 99999999999999 11111111111111 -1)
       (map #(model-number-vec %))
       (filter #(not (some zero? %)))))

(defn answer-for-part-1 []
  (let [d24 (parse (read-data "resources/d24.txt"))]
    (first (filter #(= 0 (:z (alu d24 %))) model-numbers))))