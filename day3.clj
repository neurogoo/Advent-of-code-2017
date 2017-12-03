(defn manhattan-distance
  [coord]
  (+ (Math/abs (first coord))
     (Math/abs (second coord))))

(defn next-coord
  [next-dir cur-coord]
  (cond
    (= next-dir :right) (update cur-coord 0 inc)
    (= next-dir :up) (update cur-coord 1 inc)
    (= next-dir :left) (update cur-coord 0 dec)
    (= next-dir :down) (update cur-coord 1 dec)))
;;part 1
(defn calculate-distance
  [destination]
  (loop [curid 1
         cur-coord [0 0]
         directions (mapcat #(repeat %1 %2) (interleave (iterate inc 1) (iterate inc 1)) (cycle [:right :up :left :down]))]
    (if (= curid destination)
      (manhattan-distance cur-coord)
      (recur (inc curid)
             (next-coord (first directions) cur-coord)
             (rest directions)))))

(defn calculate-cur-coord-value
  [cur-coord visited-places]
  (let [check-dirs [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
        to-coord (fn [[x y]]
                   [(+ (first cur-coord) x) (+ (second cur-coord) y)])]
    (reduce (fn [sum dir]
              (if-let [value (get visited-places (to-coord dir))]
                (+ sum value)
                sum)) 0 check-dirs)))
;;part 2
(defn calculate-value
  [target]
  (loop [curid 1
         cur-coord [0 0]
         directions (mapcat #(repeat %1 %2) (interleave (iterate inc 1) (iterate inc 1)) (cycle [:right :up :left :down]))
         visited-places {cur-coord 1}]
    (let [next-coords (next-coord (first directions) cur-coord)
          value (calculate-cur-coord-value next-coords visited-places)]
      (if (> value target)
        value
        (recur (inc curid)
               next-coords
               (rest directions)
               (assoc visited-places next-coords value))))))

