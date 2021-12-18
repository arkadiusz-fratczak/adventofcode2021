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

;Here are some examples of a single explode action:
;
;    [[[[[9,8],1],2],3],4] becomes [[[[0,9],2],3],4] (the 9 has no regular number to its left, so it is not added to any regular number).
;    [7,[6,[5,[4,[3,2]]]]] becomes [7,[6,[5,[7,0]]]] (the 2 has no regular number to its right, and so it is not added to any regular number).
;    [[6,[5,[4,[3,2]]]],1] becomes [[6,[5,[7,0]]],3].
;    [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action).
;    [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[7,0]]]].

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

;Here is the process of finding the reduced result of [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]:
;
;after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
;after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
;after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
;after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
;after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
;after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

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

;For example, the magnitude of [9,1] is 3*9 + 2*1 = 29; the magnitude of [1,9] is 3*1 + 2*9 = 21. Magnitude calculations are recursive: the magnitude of [[9,1],[1,9]] is 3*29 + 2*21 = 129.
;
;Here are a few more magnitude examples:
;
;    [[1,2],[[3,4],5]] becomes 143.
;    [[[[0,7],4],[[7,8],[6,0]]],[8,1]] becomes 1384.
;    [[[[1,1],[2,2]],[3,3]],[4,4]] becomes 445.
;    [[[[3,0],[5,3]],[4,4]],[5,5]] becomes 791.
;    [[[[5,0],[7,4]],[5,5]],[6,6]] becomes 1137.
;    [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] becomes 3488.
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

;So, given this example homework assignment:
;
;("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]")
;("[[[5,[2,8]],4],[5,[[9,9],0]]]")
;("[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]")
;("[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]")
;("[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]")
;("[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]")
;("[[[[5,4],[7,7]],8],[[8,3],8]]")
;("[[9,3],[[9,9],[6,[4,9]]]]")
;("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]")
;("[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")
;
;The final sum is:
;
;[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
;
;The magnitude of this final sum is 4140.

(defn answer []
  (let [d18 (parse (read-data-safe "resources/d18.txt"))]
    (->> d18
         (map flatten-number)
         (reduce sum-nr)
         magnitude)))

(defn answer2 []
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
