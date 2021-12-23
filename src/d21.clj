(ns d21)

(defn new-position [old-pos dice-result]
  (inc (rem (dec (+ old-pos dice-result)) 10)))

(defn round [pos score rolls-count dice]
  (let [rolls (take rolls-count dice)
        np (new-position pos (apply + (take 3 rolls)))]
    {:p np
     :s (+ score np)
     :r rolls
     :d (drop rolls-count dice)}))

(defn simulate-game [{:keys [p1 p2 s1 s2 r d] :as g} limit]
  (let [p1-move (round p1 s1 3 d)
        p2-move (round p2 s2 3 (:d p1-move))]
    (cond
      (<= limit (:s p1-move)) (merge g {:p1 (:p p1-move)
                                        :s1 (:s p1-move)
                                        :r  (+ 3 r)
                                        :d  nil})
      (<= limit (:s p2-move)) (merge g
                                     (if (> limit (:s p1-move))
                                       {:p1 (:p p1-move)
                                        :s1 (:s p1-move)})
                                     {:p2 (:p p2-move)
                                      :s2 (:s p2-move)
                                      :r  (+ 6 r)
                                      :d  nil})
      :else (simulate-game
             (merge g {:p1 (:p p1-move)
                       :s1 (:s p1-move)
                       :p2 (:p p2-move)
                       :s2 (:s p2-move)
                       :r  (+ 6 r)
                       :d  (drop 6 d)})
             limit))))

(defn answer-for-part-1 []
  (let [game {:p1 1
              :p2 3
              :s1 0
              :s2 0
              :r  0
              :d  (map #(inc (rem % 100)) (range))}
        r (simulate-game game 1000)]
    (* (:r r) (min (:s1 r) (:s2 r)))))

(def dice-results-for-single-turn
  (frequencies
   (for [throw-1 [1 2 3]
         throw-2 [1 2 3]
         throw-3 [1 2 3]]
     (+ throw-1 throw-2 throw-3))))

(defn pos-and-score-for-single-turn [{:keys [p s]} old-freq]
  (map (fn [[k v]] (let [np (new-position p k)]
                     {{:p np :s (+ s np)} (* old-freq v)}))
       dice-results-for-single-turn))

(defn qround [player]
  (if (not-empty (:pos-and-score-freq player))
    (assoc player :pos-and-score-freq
           (reduce
            (fn [acc ps]
              (apply merge-with +
                     acc
                     (pos-and-score-for-single-turn (key ps) (val ps))))
            {}
            (:pos-and-score-freq player)))
    (assoc player :pos-and-score-freq
           (apply merge-with + (pos-and-score-for-single-turn {:p (:base-pos player)
                                                               :s 0}
                                                              1)))))

(defn count-wins [player]
  (let [wins (filter (fn [[{:keys [_ s]} _]] (>= s 21)) (:pos-and-score-freq player))
        wins-count (apply + (map val wins))
        new-pos-and-score-freq (apply dissoc (:pos-and-score-freq player) (map key wins))]
    {:base-pos (:base-pos player)
     :pos-and-score-freq new-pos-and-score-freq
     :wins (+ (:wins player) wins-count)}))

(defn qsimulate [game]
  (let [p1 (count-wins (qround (:p1 game)))
        p2 (count-wins (qround (:p2 game)))]
    (cond
      (empty? (:pos-and-score-freq p1)) {:p1 p1 :p2 (:p2 game)}
      (empty? (:pos-and-score-freq p2)) {:p1 p1 :p2 p2}
      :else (recur {:p1 p1 :p2 p2}))))

(defn answer-for-part-2 []
  (let [game {:p1 {:base-pos           1
                   :pos-and-score-freq {}
                   :wins               0}
              :p2 {:base-pos           3
                   :pos-and-score-freq {}
                   :wins               0}}
        end-game (qsimulate game)]
    (max (-> end-game :p1 :wins) (-> end-game :p2 :wins))))