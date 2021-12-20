(ns d19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-data [filename]
  (->> filename
       slurp))

(defn parse-coords [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn dist-to-other-beacons [b bcs]
  (set (for [b2 bcs]
         (+ (* (- (first b2) (first b))
               (- (first b2) (first b)))
            (* (- (second b2) (second b))
               (- (second b2) (second b)))
            (* (- (last b2) (last b))
               (- (last b2) (last b)))))))

(defn parse-satelite [i s]
  (let [beacons (mapv #(parse-coords %) (str/split-lines s))]
    {:s i
     :b (mapv #(hash-map :c %
                         :d (dist-to-other-beacons % beacons)) beacons)}))

(defn parse [data]
  (->> (str/split data #"--- scanner \d{1,2} ---")
       (filter #(not-empty %))
       (map #(.trim %))
       (map-indexed parse-satelite)))

(defn matching-satellites [s sats]
  (map
   (fn [[k v]] {:s  k
                :cm (mapv :cm v)
                :b  (:b (first v))})
   (group-by :s (for [sb (:b s)
                      os sats
                      ob (:b os)
                      :when (<= 11 (count (set/intersection (:d sb) (:d ob))))]
                  {:s  (:s os)
                   :b  (:b os)
                   :cm [(:c sb) (:c ob)]}))))

(def facing-fns
  [(fn [[x y z]] [x y z])
   (fn [[x y z]] [x z y])
   (fn [[x y z]] [y x z])
   (fn [[x y z]] [y z x])
   (fn [[x y z]] [z x y])
   (fn [[x y z]] [z y x])])

(def direction-fns
  [(fn [[x y z]] [x y z])
   (fn [[x y z]] [x (- y) z])
   (fn [[x y z]] [x y (- z)])
   (fn [[x y z]] [x (- y) (- z)])
   (fn [[x y z]] [(- x) y z])
   (fn [[x y z]] [(- x) (- y) z])
   (fn [[x y z]] [(- x) y (- z)])
   (fn [[x y z]] [(- x) (- y) (- z)])])

(def rotation-fns
  (for [ffn facing-fns
        dfn direction-fns]
    (comp ffn dfn)))

(defn subtraction [[x2 y2 z2] [x1 y1 z1]]
  [(- x2 x1) (- y2 y1) (- z2 z1)])

(defn shift-vector [p1 p2 rfn]
  (subtraction (rfn p2) p1))

(defn shift-data [all-possible-shifts]
  (let [sd (first (filter #(= 1 (count (:sh %))) all-possible-shifts))]
    {:fn (:fn sd)
     :sh (first (:sh sd))}))

(defn shift-fn [coords-mapping]
  (let [all-possible-shifts
        (map
         (fn [rfn]
           {:fn rfn
            :sh (distinct (map (fn [[p1 p2]] (shift-vector p1 p2 rfn)) coords-mapping))})
         rotation-fns)
        shift-data (shift-data all-possible-shifts)]
    {:fn (fn [p] (subtraction ((:fn shift-data) p) (:sh shift-data)))
     :sh (:sh shift-data)}))

(defn discover-all-beacons [master-satellite other-satellites]
  (loop [m master-satellite
         o other-satellites]
    (if (empty? o)
      (:b m)
      (let [ms (matching-satellites m o)
            msk (set (map :s ms))
            new-others (remove #(contains? msk (:s %)) o)
            add-beacons (reduce
                         (fn [acc fs]
                           (let [sh-fn (:fn (shift-fn (:cm fs)))]
                             (concat acc (mapv #(hash-map :c (sh-fn (:c %))
                                                          :d (:d %)) (:b fs)))))
                         []
                         ms)
            new-m (update m :b concat add-beacons)
            _ (println (count add-beacons) (count (:b new-m)) (count new-others))]
        (recur new-m new-others)))))

(defn answer-part-1 []
  (let [d19 (parse (read-data "resources/d19.txt"))
        first-satellite (first d19)
        other-satellites (rest d19)]
    (count
     (distinct
      (map :c (discover-all-beacons first-satellite other-satellites))))))

(defn discover-coords-of-all-satellites [master-satellite other-sattelites]
  (loop [m master-satellite
         o other-sattelites
         d [(assoc m :c [0 0 0])]]
    (if (empty? o)
      (map #(hash-map :s (:s %)
                      :c (:c %)) d)
      (let [ms (matching-satellites m o)
            new-others (remove #(contains? (set (map :s ms)) (:s %)) o)
            adjusted-satellites (reduce
                                 (fn [acc fs]
                                   (let [sh-data (shift-fn (:cm fs))]
                                     (concat acc [{:s (:s fs)
                                                   :c (:sh sh-data)
                                                   :b (mapv #(hash-map :c ((:fn sh-data) (:c %))
                                                                       :d (:d %)) (:b fs))}])))
                                 []
                                 ms)
            new-m (update m :b concat (apply concat (map :b adjusted-satellites)))
            new-d (concat d adjusted-satellites)
            _ (println (count (:b new-m)) (count new-others) (count new-d))]
        (recur new-m new-others new-d)))))

(defn manhattan-dist [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))
     (Math/abs (- z2 z1))))

(defn answer-part-2 []
  (let [d19 (parse (read-data "resources/d19.txt"))
        first-satellite (first d19)
        other-satellites (rest d19)
        aligned-coords (map :c (discover-coords-of-all-satellites first-satellite other-satellites))]
    (apply max
           (for [c aligned-coords
                 c2 aligned-coords
                 :when (not= c c2)]
             (manhattan-dist c c2)))))