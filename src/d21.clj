(ns d21)

(defn turn [pos score rolls-count dice]
  (let [rolls (take rolls-count dice)
        np (inc (rem (dec (+ pos (apply + (take 3 rolls)))) 10))]
    {:p np
     :s (+ score np)
     :r rolls
     :d (drop rolls-count dice)}))

(defn simulate-game [{:keys [p1 p2 s1 s2 r d] :as g} limit]
  (let [p1-move (turn p1 s1 3 d)
        p2-move (turn p2 s2 3 (:d p1-move))]
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

(defn answer-for-part-2 []
  (let [game {:p1 1
              :p2 3
              :s1 0
              :s2 0
              :r  0
              :d  (map #(inc (rem % 100)) (range))}
        r (simulate-game game 21)]
    (* (:r r) (min (:s1 r) (:s2 r)))))