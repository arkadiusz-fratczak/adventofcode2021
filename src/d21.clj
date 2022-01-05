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

(def quantum-dice
  (frequencies
   (for [throw-1 [1 2 3]
         throw-2 [1 2 3]
         throw-3 [1 2 3]]
     (+ throw-1 throw-2 throw-3))))

(defn roll-quantum-dice [[board multiverses] player-key]
  (map (fn [[roll mv]]
         (let [player-pos (new-position (-> board player-key :pos) roll)
               player-score (+ (-> board player-key :score) player-pos)]
           {(merge board
                   {player-key {:pos player-pos
                                :score player-score}}) (*' multiverses mv)}))
       quantum-dice))

(defn quantum-round [multiverse-boards player-key]
  (reduce
   (fn [new-multiverse-boards multi-board]
     (apply merge-with +'
            new-multiverse-boards
            (roll-quantum-dice multi-board player-key)))
   {}
   multiverse-boards))

(defn separate-map [pred m]
  ((juxt (comp (partial into {}) filter) (comp (partial into {}) remove)) pred m))

(defn count-wins [game new-multiverse-boards player-key]
  (let [[boards-to-remove boards-to-keep] (separate-map #(< 20 (get-in (key %) [player-key :score]))
                                                        new-multiverse-boards)
        wins (apply + (map val boards-to-remove))
        player-wins-key (keyword (str (name player-key) "-wins"))]
    (assoc
     (update game player-wins-key +' wins)
     :boards boards-to-keep)))

(defn quantum-simulate [game]
  (let [boards-after-p1-move (quantum-round (:boards game) :p1)
        game-after-p1-move (count-wins game boards-after-p1-move :p1)
        boards-after-p2-move (quantum-round (:boards game-after-p1-move) :p2)
        game-after-p2-move (count-wins game-after-p1-move boards-after-p2-move :p2)]
    (if (empty? (:boards game-after-p2-move))
      game-after-p2-move
      (recur game-after-p2-move))))

(defn answer-for-part-2 []
  (let [game {:p1-wins 0
              :p2-wins 0
              :boards {{:p1 {:pos 1
                             :score 0}
                        :p2 {:pos 3
                             :score 0}} 1}}
        end-game (quantum-simulate game)]
    (max (:p1-wins end-game) (:p2-wins end-game))))