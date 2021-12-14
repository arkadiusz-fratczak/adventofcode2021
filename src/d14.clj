(ns d14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;--- Day 14: Extended Polymerization ---
;
;The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.
;
;The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.
;
;For example:
;
;"NNCB"
;""
;"CH -> B"
;"HH -> N"
;"CB -> H"
;"NH -> C"
;"HB -> C"
;"HC -> B"
;"HN -> C"
;"NN -> C"
;"BH -> H"
;"NC -> B"
;"NB -> B"
;"BN -> B"
;"BB -> N"
;"BC -> B"
;"CC -> N"
;"CN -> C"
;
;The first line is the polymer template - this is the starting point of the process.
;
;The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.
;
;So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:
;
;    The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
;    The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
;    The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.
;
;Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.
;
;After the first step of this process, the polymer becomes NCNBCHB.
;
;Here are the results of a few steps using the above rules:
;
;Template:     NNCB
;After step 1: NCNBCHB
;After step 2: NBCCNBBBCBHCB
;After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
;After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
;
;This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.
;
;Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

(defn read-data-safe [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec)))

(defn parse [data]
  (->> data
       (reduce
        (fn [acc line]
          (cond-> acc
            (not-empty (re-seq #"^\w+$" line))
            (assoc :template (first (re-seq #"^\w+$" line)))

            (not-empty (re-seq #"(\w+) \-> (\w+)" line))
            (assoc-in [:rules (second (first (re-seq #"(\w+) \-> (\w+)" line)))]
                      (let [key (second (first (re-seq #"(\w+) \-> (\w+)" line)))
                            val (last (first (re-seq #"(\w+) \-> (\w+)" line)))]
                        (str
                                 ;(first key)
                         val
                         (last key))))))
        {})))

(defn perform-step [template rules]
  (->> template
       (partition 2 1)
       (mapv #(apply str %))
       (mapv (fn [polymer]
               (if (contains? rules polymer)
                 (get rules polymer)
                 (last polymer))))
       (apply str (first template))))

(defn perform-steps [template rules steps]
  (reduce
   (fn [r _] (perform-step r rules))
   template
   (repeat steps 0)))

(defn answer []
  (let [d14 (parse (read-data-safe "resources/d14.txt"))]
    (->> d14
         (#(perform-steps (:template %) (:rules %) 10))
         frequencies
         vals
         sort
         (#(- (last %) (first %))))))

;--- Part Two ---
;
;The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.
;
;In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.
;
;Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?


(defn compute-step10-for-rules [rules]
  (->> rules
       keys
       (map #(hash-map % (perform-steps % rules 10)))
       (apply merge)))

(defn perform-10steps [template rules-step10]
  ;(loop [n 1
  ;       t template
  ;       f {}]
  ;  (if (<= n 0)
  ;    f
  ;    (let [partitions (map #(apply str %) (partition 2 1 template))]
  ;      )))
  (->> template
       (partition 2 1)
       (map #(get rules-step10 (apply str %)))
       ;(reduce (fn [p1 p2] {:t (apply str (:t p1) (next (:t p2)))
       ;                     :f (merge-with + (:f p1) (update (:f p2) (first (:t p2)) dec))}))
       ;(apply str)
       ))

(defn answer2 []
  (let [d14 (parse (read-data-safe "resources/d14.txt"))
        rules-step10 (compute-step10-for-rules (:rules d14))]
    (->> "SB"
         (#(perform-10steps % rules-step10))
         (map #(perform-10steps % rules-step10))
         flatten
         (map #(perform-10steps % rules-step10))
         flatten
         (map #(perform-10steps % rules-step10))
         flatten
         (apply concat)
         frequencies
         ;vals
         ;sort
         ;(#(- (last %) (first %)))
         )))