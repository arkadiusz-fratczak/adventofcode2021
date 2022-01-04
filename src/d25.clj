(ns d25
  (:require [clojure.java.io :as io]))

(defn read-data [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn herd-locations [data direction]
  (set (for [row (range (count data))
             col (range (count (first data)))
             :when (= direction (nth (nth data row) col))]
         [col row])))

(defn parse [data]
  {:rows       (count data)
   :cols       (count (first data))
   :east-herd  (herd-locations data \>)
   :moved-east 0
   :south-herd (herd-locations data \v)
   :moved-south 0})

(defn east [max-x [x y]]
  [(mod (inc x) max-x) y])

(defn south [max-y [x y]]
  [x (mod (inc y) max-y)])

(defn move [herd-to-move other-herd new-loc-fn]
  (reduce
   (fn [acc loc]
     (let [new-loc (new-loc-fn loc)]
       (if (or (contains? herd-to-move new-loc)
               (contains? other-herd new-loc))
         (update acc :herd conj loc)
         (update (update acc :herd conj new-loc)
                 :moved inc))))
   {:herd #{}
    :moved 0}
   herd-to-move))

(defn step [{:keys [rows cols east-herd south-herd] :as grid}]
  (let [new-east-herd (move east-herd south-herd (partial east cols))
        new-south-herd (move south-herd (:herd new-east-herd) (partial south rows))]
    (merge grid
           {:east-herd (:herd new-east-herd)
            :moved-east (:moved new-east-herd)}
           {:south-herd (:herd new-south-herd)
            :moved-south (:moved new-south-herd)})))

(defn find-stop-moving-step [grid]
  (loop [g (step grid)
         s 1]
    (if (and (zero? (:moved-east g))
             (zero? (:moved-south g)))
      s
      (recur (step g) (inc s)))))

(defn answer-for-part-1 []
  (let [d25 (parse (read-data "resources/d25.txt"))]
    (find-stop-moving-step d25)))