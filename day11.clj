(defn parse-direction-input
  [f]
  (clojure.string/split (clojure.string/trim-newline (slurp f)) #","))

(def hex-coords {:n [0 1]
                 :ne [1 1]
                 :se [1 0]
                 :s [0 -1]
                 :sw [-1 -1]
                 :nw [-1 0]})

(defn walk-through-hex
  [route]
  (reduce (fn [[[x y] dis] n]
            (let [[add_x add_y] ((keyword n) hex-coords)
                  [new_x new_y] [(+ x add_x) (+ y add_y)]]
              [[new_x new_y] (max dis (hex-distance [new_x new_y]))]))
          [[0 0] 0] route))

(defn hex-distance
  [[x y]]
  (max (Math/abs x) (Math/abs y) (Math/abs (- x y))))
