;; This buffer is for Clojure experiments and evaluation.
;; Press C-j to evaluate the last expression.
(defn read-input
  [f]
  (-> f
      slurp
      clojure.string/trim-newline
      (clojure.string/split #",")))

(defn spin
  [pos x]
  (vec (concat (take-last x pos) (vec (drop-last x pos)))))

(defn exchange
  [pos a b]
  (let [a-pos (nth pos a)
        b-pos (nth pos b)]
    (-> pos
        (assoc a b-pos)
        (assoc b a-pos))))
(defn partner
  [pos a b]
  (let [a-index (.indexOf pos a)
        b-index (.indexOf pos b)]
    (exchange pos a-index b-index)))
(defn dance
  [patterns start]
  (reduce (fn [pos move]
            (let [[command & r] move]
              (case command
                \s (spin pos (Integer/parseInt (apply str r)))
                \x (let [[a b] (map #(Integer/parseInt %)
                                    (clojure.string/split (apply str r) #"/"))]
                     (exchange pos a b))
                \p (let [[a _ b] r]
                     (partner pos a b)))))
          start patterns))

;;Calculate the loop size of the dance routine and do the dance
(defn repeat-dance
  [patterns times]
  (reduce (fn [pos n]
            (let [next (dance patterns pos)]
              (if (= next (vec "abcdefghijklmnop"))
                (reduced n)
                next)))
          (vec "abcdefghijklmnop") (range 0 times)))
