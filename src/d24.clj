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

(defn alu [ins state]
  (reduce
   (fn [acc in]
     (case (first in)
       :inp (inp (last in) acc)
       :add (add (last in) acc)
       :mul (mul (last in) acc)
       :div (div (last in) acc)
       :mod (modulo (last in) acc)
       :eql (eql (last in) acc)))
   state
   ins))

(defn base-state [data]
  {:w    0
   :x    0
   :y    0
   :z    0
   :data data})

(defn group-ins [ins]
  (->> ins
       (reduce
        (fn [acc ins]
          (if (= :inp (first ins))
            (conj acc [ins])
            (conj (pop acc) (conj (peek acc) ins))))
        [])))

(defn add-digit [numbers]
  (if (empty? numbers)
    (map vector (range 1 10))
    (for [nr numbers
          d (range 1 10)]
      (conj nr d))))

(defn some-variable-zero? [state]
  (some zero? [(:x state) (:y state) (:z state)]))

(defn run-monad
  ([ins-set]
   (run-monad ins-set (add-digit [])))
  ([ins-set numbers]
   (let [input-size (count (first numbers))
         new-ins-set (apply concat (take input-size ins-set))
         filtered-numbers (filterv #(some-variable-zero? (alu new-ins-set (base-state %))) numbers)
         _ (println "lvl:" input-size "nbrs-size:" (count numbers) "filtered-nbrs-size:" (count filtered-numbers))]
     (if (< 13 input-size)
       filtered-numbers
       (if (empty? filtered-numbers)
         (recur ins-set (add-digit numbers))
         (recur ins-set (add-digit filtered-numbers)))))))

(defn answer-for-part-1-and-2 []
  (let [d24 (parse (read-data "resources/d24.txt"))
        ins-set (group-ins d24)
        valid-numbers (time (run-monad ins-set))]
    [(apply str (last valid-numbers))
     (apply str (first valid-numbers))]))
