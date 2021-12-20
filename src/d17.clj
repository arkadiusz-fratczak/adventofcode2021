(ns d17)

(defn in-target [[x y] target]
  (and (<= (first (:x target)) x (last (:x target)))
       (<= (first (:y target)) y (last (:y target)))))

(defn never-reach-target [[x y] [x-vel y-vel] target]
  (or (< y (first (:y target)))
      (and (= x-vel 0)
           (or
            (> x (last (:x target)))
            (< x (first (:x target)))))))

(defn step [{[x y] :p [x-vel y-vel] :v}]
  {:p [(+ x x-vel) (+ y y-vel)]
   :v [(cond
         (> 0 x-vel) (inc x-vel)
         (< 0 x-vel) (dec x-vel)
         :else x-vel)
       (dec y-vel)]})

(defn simulate [vel target]
  (loop [d {:p [0 0]
            :v vel}
         s [d]]
    (let [r (step d)
          ns (conj s r)]
      (cond
        (in-target (:p r) target) {:hit true
                                   :steps ns
                                   :max-y (apply max (map #(last (:p %)) ns))}
        (never-reach-target (:p r) (:v r) target) {:hit false
                                                   :steps ns
                                                   :max-y (apply max (map #(last (:p %)) ns))}
        :else (recur r ns)))))

(defn hitting-simulations-for-x [x-vel target]
  (loop [y-vel (first (:y target))
         sims []]
    (let [sim (simulate [x-vel y-vel] target)]
      (if (> y-vel (- (first (:y target))))
        sims
        (recur (inc y-vel)
               (if (:hit sim) (conj sims sim) sims))))))

(defn possible-x-vel-values [[x-min x-max]]
  (filter
   (fn [x] (loop [v x
                  acc 0]
             (if (and (< acc x-min) (> v 0))
               (recur (dec v) (+ acc v))
               (<= x-min acc x-max))))
   (range (inc x-max))))

(defn hitting-simulations [target]
  (loop [x-vel (first (possible-x-vel-values (:x target)))
         x-rest (next (possible-x-vel-values (:x target)))
         sims []]
    (if (nil? x-vel)
      sims
      (recur (first x-rest)
             (next x-rest)
             (concat sims (hitting-simulations-for-x x-vel target))))))

(defn answer-part-1 []
  (let [target {:x [138 184]
                :y [-125 -71]}]
    (->> (hitting-simulations target)
         (map :max-y)
         (apply max))))

(defn answer-part-2 []
  (let [target {:x [138 184]
                :y [-125 -71]}]
    (->> (hitting-simulations target)
         count)))