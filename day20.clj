(defn parse-particle
  [l]
  (let [[p v a] (clojure.string/split l #", ")]
    {:pos (map #(Integer/parseInt %) (re-seq #"-?\d+" p))
     :vel (map #(Integer/parseInt %) (re-seq #"-?\d+" v))
     :acc (map #(Integer/parseInt %) (re-seq #"-?\d+" a))}))

(defn read-particle-data
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map #(parse-particle %))))

(defn calculate-manhattan-distance
  [[x y z]]
  (+ (Math/abs x)
     (Math/abs y)
     (Math/abs z)))

(defn smallest-accelaration
  [particles]
  (first (first (sort-by second (map-indexed #(list %1 (calculate-manhattan-distance (:acc %2))) particles)))))

(defn increase-velocities
  [particles]
  (map #(let [{v :vel a :acc :as all} %]
          (assoc all :vel (list (+ (nth v 0) (nth a 0))
                                (+ (nth v 1) (nth a 1))
                                (+ (nth v 2) (nth a 2))))) particles))
(defn increase-positions
  [particles]
  (map #(let [{v :vel p :pos :as all} %]
          (assoc all :pos (list (+ (nth v 0) (nth p 0))
                                (+ (nth v 1) (nth p 1))
                                (+ (nth v 2) (nth p 2))))) particles))
(defn check-collisions
  [particles]
  (let [groups (group-by :pos particles)
        particles (flatten (map second (filter (fn [[pos p]]
                                        (= 1 (count p))) groups)))]
    particles))

(defn evolve-system
  [particles rounds]
  (loop [particles particles
         n 0]
    (if (>= n rounds)
      "over"
      (let [particles (increase-velocities particles)
            particles (increase-positions particles)
            particles (check-collisions particles)]
        (println (str "Particle count is " (count particles)))
        (recur particles (inc n))))))
