(defn clean-data
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map (fn [l] (map (fn [i] (Integer/parseInt i)) (clojure.string/split l #"\t"))))))

(defn checksum
  [lines]
  (reduce +
          (map #(- (apply max %)
                   (apply min %)) lines)))

(defn divisables
  [lines]
  (reduce +
          (map (fn [l]
                 (first
                  (remove nil? (for [x l
                                     y l]
                               (if (and (not= x y)
                                        (= 0 (mod x y)))
                                 (/ x y)))))) lines)))
