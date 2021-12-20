(ns d14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn answer-part-1 []
  (let [d14 (parse (read-data-safe "resources/d14.txt"))]
    (->> d14
         (#(perform-steps (:template %) (:rules %) 20))
         frequencies
         vals
         sort
         (#(- (last %) (first %))))))

(defn compute-N-step-for-rules [rules nstep]
  (->> rules
       keys
       (map #(let [stepN (perform-steps % rules nstep)]
               {% {:t stepN
                   :f (frequencies stepN)}}))
       (apply merge)))

(defn perform-N-steps [template rules-n-step]
  (->> (or (:t template) template)
       (partition 2 1)
       (map #(get rules-n-step (apply str %)))))

(defn answer-part-2 []
  (let [d14 (parse (read-data-safe "resources/d14.txt"))
        rules-step20 (compute-N-step-for-rules (:rules d14) 20)]
    (->> (:template d14)
         (#(perform-N-steps % rules-step20))
         (map #(perform-N-steps % rules-step20))
         flatten
         (reduce (fn [acc p1] (merge-with + acc (update (:f p1) (first (:t p1)) dec)))
                 {})
         vals
         sort
         (#(- (last %) (first %))))))