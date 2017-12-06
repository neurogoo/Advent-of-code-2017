(defn clean-block-data
  [f]
  (vec (map #(Integer/parseInt %)
            (-> f
                slurp
                clojure.string/trim-newline
                (clojure.string/split #"\t")))))

(defn redistribute
  [banks]
  (let [bank-size (count banks)
        max-val (apply max banks)
        max-index (first
                   (first
                    (filter #(= (second %) max-val)
                            (map-indexed (fn [i item] [i item]) banks))))]
    (loop [new-banks (assoc banks max-index 0)
           cur-index (inc max-index)
           cur-val max-val]
      (if (= cur-val 0)
        new-banks
        (let [new-index (if (>= cur-index bank-size)
                          0
                          cur-index)]
          (recur (update new-banks new-index inc)
                 (inc new-index)
                 (dec cur-val)))))))

;;return [cycles cycle-start-index]
(defn check-looping-memory
  [banks]
  (loop [cur banks
         prevs #{}
         prev-map {}
         cycles 0]
    (if (prevs cur)
      [cycles (get prev-map cur)]
      (recur (redistribute cur)
             (conj prevs cur)
             (assoc prev-map cur cycles)
             (inc cycles)))))
