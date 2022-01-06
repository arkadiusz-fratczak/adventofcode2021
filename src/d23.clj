(ns d23
  (:require [clojure.set :as set]))

(def step-costs
  {"A" 1
   "B" 10
   "C" 100
   "D" 1000})

(def room-entry
  {"A" 2
   "B" 4
   "C" 6
   "D" 8})

(defn room-available? [burrow amph]
  (let [room (get-in burrow [:rooms amph])]
    (or (empty? room)
        (every? #(= % amph) room))))

(defn hall-not-blocked? [burrow amph hall-pos]
  (let [room-entry (room-entry amph)
        path (range room-entry hall-pos (if (< room-entry hall-pos) 1 -1))
        hall (:hall burrow)]
    (every? #(nil? (hall %)) path)))

(defn hall-to-room-movement-cost [burrow amph hall-pos]
  (let [room-entry (room-entry amph)
        path-to-room-cost (* (step-costs amph) (Math/abs (- hall-pos room-entry)))
        room-cost (* (step-costs amph) (- (:room-cap burrow) (count (get-in burrow [:rooms amph]))))]
    (+ path-to-room-cost room-cost)))

(defn to-room [burrow amph hall-pos]
  (if (and (room-available? burrow amph)
           (hall-not-blocked? burrow amph hall-pos))
    (let [new-hall (dissoc (:hall burrow) hall-pos)
          new-rooms (:rooms (update-in burrow [:rooms amph] conj amph))
          new-cost (+ (:cost burrow) (hall-to-room-movement-cost burrow amph hall-pos))]
      {:rooms new-rooms
       :hall new-hall
       :cost new-cost
       :room-cap (:room-cap burrow)
       ;:steps (conj (:steps burrow) [(str hall-pos amph) amph])
       })))

(defn from-hall-to-rooms [burrow]
  (reduce
   (fn [new-burrows [hall-pos amph]]
     (if-let [room (to-room burrow amph hall-pos)]
       (conj new-burrows room)
       new-burrows))
   []
   (:hall burrow)))

(defn room-contains-others? [burrow room-name]
  (let [room (get-in burrow [:rooms room-name])]
    (and (not-empty room)
         (some #(not= % room-name) room))))

(defn possible-hall-places [burrow room-name]
  (let [room-entry-place (room-entry room-name)
        occupied-hall-places (keys (:hall burrow))
        first-left-occupied (apply max (if-let [s (seq (filter #(< % room-entry-place) occupied-hall-places))] s [-1]))
        first-right-occupied (apply min (if-let [s (seq (filter #(> % room-entry-place) occupied-hall-places))] s [11]))]
    (remove #(contains? (set (vals room-entry)) %) (range (inc first-left-occupied) first-right-occupied))))

(defn room-to-hall-movement-cost [burrow room-name amph hall-pos]
  (let [room-entry (room-entry room-name)
        room-cost (* (step-costs amph) (- (:room-cap burrow) (count (get-in burrow [:rooms room-name]))))
        path-to-hall-cost (* (step-costs amph) (inc (Math/abs (- hall-pos room-entry))))]
    (+ path-to-hall-cost room-cost)))

(defn to-hall [burrow room-name]
  (if (room-contains-others? burrow room-name)
    (let [amph (first (get-in burrow [:rooms room-name]))
          places (possible-hall-places burrow room-name)]
      (if-not (room-available? burrow amph)
        (reduce
         (fn [new-burrows place]
           (conj new-burrows
                 {:rooms (:rooms (update-in burrow [:rooms room-name] (comp vec rest)))
                  :hall (merge (:hall burrow) {place amph})
                  :cost (+ (:cost burrow) (room-to-hall-movement-cost burrow room-name amph place))
                  :room-cap (:room-cap burrow)
                  ;:steps (conj (:steps burrow) [room-name (str place amph)])
                  }))
         []
         places)))))

(defn from-rooms-to-hall [burrow]
  (concat (to-hall burrow "D")
          (to-hall burrow "C")
          (to-hall burrow "B")
          (to-hall burrow "A")))

(defn room-to-room-movement-cost [burrow room-name amph]
  (let [room-cap (:room-cap burrow)
        source-room-entry (room-entry room-name)
        target-room-entry (room-entry amph)
        source-room-cost (* (step-costs amph) (- room-cap (count (get-in burrow [:rooms room-name]))))
        target-room-cost (* (step-costs amph) (- room-cap (count (get-in burrow [:rooms amph]))))
        hall-path-cost (* (step-costs amph) (inc (Math/abs (- source-room-entry target-room-entry))))]
    (+ source-room-cost hall-path-cost target-room-cost)))

(defn room-to-room [burrow room-name]
  (if (room-contains-others? burrow room-name)
    (let [amph (first (get-in burrow [:rooms room-name]))]
      (if (and (room-available? burrow amph)
               (hall-not-blocked? burrow amph (room-entry room-name)))
        [{:rooms (:rooms (update-in (update-in burrow [:rooms room-name] (comp vec rest))
                                    [:rooms amph] conj amph))
          :hall (:hall burrow)
          :cost (+ (:cost burrow) (room-to-room-movement-cost burrow room-name amph))
          :room-cap (:room-cap burrow)
          ;:steps (conj (:steps burrow) [room-name amph])
          }]))))

(defn from-rooms-to-room [burrow]
  (if-let [d (seq (room-to-room burrow "D"))]
    d
    (if-let [c (seq (room-to-room burrow "C"))]
      c
      (if-let [b (seq (room-to-room burrow "B"))]
        b
        (room-to-room burrow "A")))))

(defn next-burrows [burrow]
  (if-let [room-to-room (seq (from-rooms-to-room burrow))]
    (vec room-to-room)
    (if-let [hall-to-room (seq (from-hall-to-rooms burrow))]
      (vec hall-to-room)
      (vec (from-rooms-to-hall burrow)))))

(defn solved? [burrow]
  (and (= (repeat (:room-cap burrow) "A") (get-in burrow [:rooms "A"]))
       (= (repeat (:room-cap burrow) "B") (get-in burrow [:rooms "B"]))
       (= (repeat (:room-cap burrow) "C") (get-in burrow [:rooms "C"]))
       (= (repeat (:room-cap burrow) "D") (get-in burrow [:rooms "D"]))))

(defn solve [burrow]
  (loop [to-process [burrow]
         processed #{}
         min-cost-solved Integer/MAX_VALUE]
    (println (count to-process) (count processed) min-cost-solved)
    (if (empty? to-process)
      min-cost-solved
      (let [burrow (first to-process)
            rest-burrows (rest to-process)
            new-processed (conj processed burrow)]
        (if (solved? burrow)
          (recur rest-burrows
                 new-processed
                 (min min-cost-solved (:cost burrow)))
          (recur (set (concat rest-burrows
                              (remove #(or (>= (:cost %) min-cost-solved)
                                           (new-processed %))
                                      (next-burrows burrow))))
                 new-processed
                 min-cost-solved))))))

;#############
;#...........#
;###A#C#B#D###
;  #B#A#D#C#
;  #########
;0 112308 11332
;"Elapsed time: 396941.060952 msecs"
(defn answer-for-part-1 []
  (let [burrow {:rooms {"A" ["A" "B"]
                        "B" ["C" "A"]
                        "C" ["B" "D"]
                        "D" ["D" "C"]}
                :hall {}
                :cost 0
                :room-cap 2
                ;:steps #{}
                }]
    (time (solve burrow))))

;#############
;#...........#
;###A#C#B#D###
;  #D#C#B#A#
;  #D#B#A#C#
;  #B#A#D#C#
;  #########
;0 136634 49936
;"Elapsed time: 753434.149136 msecs"
(defn answer-for-part-2 []
  (let [burrow {:rooms {"A" ["A" "D" "D" "B"]
                        "B" ["C" "C" "B" "A"]
                        "C" ["B" "B" "A" "D"]
                        "D" ["D" "A" "C" "C"]}
                :hall {}
                :cost 0
                :room-cap 4
                ;:steps #{}
                }]
    (time (solve burrow))))
