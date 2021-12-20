(ns d18
  (:require [clojure.java.io :as io]))

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (map read-string)))

(defn flatten-number
  ([node]
   (flatten-number node 0))
  ([node lvl]
   (if (vector? node)
     (vec (concat (flatten-number (first node) (inc lvl)) (flatten-number (last node) (inc lvl))))
     [{:l lvl :v node}])))

(defn explode [n]
  (if (some #(< 4 (:l %)) n)
    (let [left-idx (:i (first (filter #(< 4 (:l %)) (map-indexed (fn [i v] {:l (:l v) :i i}) n))))
          val-left (:v (nth n left-idx))
          val-right (:v (nth n (inc left-idx)))]
      (cond
        (= 0 left-idx) (vec (concat [{:l 4 :v 0} {:l (:l (nth n (+ left-idx 2))) :v (+ val-right (:v (nth n (+ left-idx 2))))}]
                                    (drop 3 n)))
        (= (- (count n) 2) left-idx) (vec (concat (drop-last 3 n)
                                                  [{:l (:l (nth n (dec left-idx))) :v (+ val-left (:v (nth n (dec left-idx))))} {:l 4 :v 0}]))
        :else (vec (concat (take (dec left-idx) n)
                           [{:l (:l (nth n (dec left-idx)))
                             :v (+ (:v (nth n (dec left-idx))) val-left)}
                            {:l 4 :v 0}
                            {:l (:l (nth n (+ left-idx 2)))
                             :v (+ (:v (nth n (+ left-idx 2))) val-right)}]
                           (take-last (- (count n) (+ left-idx 3)) n)))))
    n))

(defn split [n]
  (if (some #(< 9 (:v %)) n)
    (let [idx (:i (first (filter #(< 9 (:v % n)) (map-indexed (fn [i v] {:v (:v v) :i i}) n))))
          val-idx (nth n idx)]
      (vec (concat (take idx n)
                   [{:l (inc (:l val-idx))
                     :v (quot (:v val-idx) 2)}
                    {:l (inc (:l val-idx))
                     :v (quot (inc (:v val-idx)) 2)}]
                   (take-last (- (count n) (inc idx)) n))))
    n))

(defn reduce-nr [n]
  (if (some #(< 4 (:l %)) n)
    (reduce-nr (explode n))
    (if (some #(< 9 (:v %)) n)
      (reduce-nr (split n))
      n)))

(defn sum-nr [n1 n2]
  (reduce-nr
   (vec (concat (map #(update-in % [:l] inc) n1)
                (map #(update-in % [:l] inc) n2)))))

(defn magnitude [n]
  (if (empty? (rest n))
    (:v (first n))
    (loop [s []
           a (first n)
           b (second n)
           r (drop 2 n)]
      (if (= (:l a) (:l b))
        (magnitude (vec (concat s
                                [{:l (dec (:l a))
                                  :v (+ (* (:v a) 3) (* (:v b) 2))}]
                                r)))
        (recur (conj s a) b (first r) (rest r))))))

(defn answer-part-1 []
  (let [d18 (parse (read-data-safe "resources/d18.txt"))]
    (->> d18
         (map flatten-number)
         (reduce sum-nr)
         magnitude)))

(defn answer-part-2 []
  (let [d18 (parse (read-data-safe "resources/d18.txt"))
        d18f (map flatten-number d18)
        to-check (for [a d18f
                       b d18f
                       :when (not= a b)]
                   [a b])]
    (->> to-check
         (map #(sum-nr (first %) (last %)))
         (map magnitude)
         (apply max))))
