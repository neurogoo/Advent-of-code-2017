(defn clean-jump-data
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map #(Integer/parseInt %))
      vec))

(defn jump-through-instructions
  [instructions]
  (loop [ins instructions
         i 0
         cycles 0]
    (let [cur-ins (nth ins i)
          next-i (+ i cur-ins)]
      (if (>= next-i (count instructions))
        (inc cycles)
        (recur (update ins i inc)
               next-i
               (inc cycles))))))

(defn jump-through-strange-instructions
  [instructions]
  (loop [ins instructions
         i 0
         cycles 0]
    (let [cur-ins (nth ins i)
          next-i (+ i cur-ins)]
      (if (>= next-i (count instructions))
        (inc cycles)
        (recur (update ins i #(if (>= % 3)
                                (dec %)
                                (inc %)))
               next-i
               (inc cycles))))))

