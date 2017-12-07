(defn convert-line-to-node-map
  [[name weight _ & neighbours]]
  {:name name
   :weight (Integer/parseInt (clojure.string/replace weight #"[(,)]" ""))
   :neighbours (map (fn [s] (clojure.string/replace s #"," "")) neighbours)})

(defn clean-disk-data
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map #(clojure.string/split % #" "))
      (map #(convert-line-to-node-map %))))

;part 1
(defn neighbour-mapping
  [l]
  (let [members (map #(:name %) l)]
    (filter #(= 0 (second %))
            (map (fn [item]
                   [item
                    (count
                     (filter true? (map #(some (fn [x] (= x item)) (:neighbours %)) l)))]) members))))

(defn to-map
  [l]
  (reduce #(assoc %1 (keyword (:name %2)) {:weight (:weight %2)
                                           :neighbours (:neighbours %2)})
          {}
          l))

(defn to-tree
  [node l]
  (let [neigh (:neighbours (node l))
        weight (:weight (node l))]
    (cons [node weight] (map #(to-tree (keyword %) l) neigh))))

(defn total-weights
  [tree]
  (let [cur-weight (second (first tree))]
    (if (= 1 (count tree))
      cur-weight
      (+ cur-weight (reduce + (map total-weights (rest tree)))))))

;;part 2: Gives wrong node and previous level weights
(defn find-wrong-node
  [tree upper-level]
  (let [weights
        (map #(list (first (first %)) (total-weights %)) (rest tree))
        wrong (filter (fn [[n s]](= 1 (count s))) (group-by second weights))]
    (if (seq wrong)
      (find-wrong-node (first
                (filter (fn [[eka & _]]
                         (= (first eka) (first (first (first (rest (first wrong))))))) (rest tree))) weights)
      [tree upper-level])))
