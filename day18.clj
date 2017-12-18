;; This buffer is for Clojure experiments and evaluation.
;; Press C-j to evaluate the last expression.

(defn read-program
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map #(clojure.string/split % #" "))))

(defn run-program
  [ins]
  (let [ins-size (count ins)]
    (loop [cur-index 0
           regs {}
           sound 0]
      (if (or (< cur-index 0) (>= cur-index ins-size))
        sound
        (let [[op x y] (nth ins cur-index)
              get-val (fn [y] (if (not (nil? y))
                                (if (and (>= (int (first y)) 97)
                                         (<= (int (first y)) 122))
                                  (or ((keyword y) regs) 0)
                                  (Integer/parseInt y))))
              y (get-val y)]
          (case opw
            "snd" (recur (inc cur-index) regs (get-val x))
            "set" (recur (inc cur-index) (assoc regs (keyword x) y) sound)
            "add" (recur (inc cur-index) (update regs (keyword x) #(+ y (or % 0))) sound)
            "mul" (recur (inc cur-index) (update regs (keyword x) #(* (or % 0) y)) sound)
            "mod" (recur (inc cur-index) (update regs (keyword x) #(mod (or % 0) y)) sound)
            "rcv" (recur (if (> (or ((keyword x) regs) 0) 0) -1 (inc cur-index)) regs sound)
            "jgz" (recur (if (> (or ((keyword x) regs) 0) 0) (+ cur-index y) (inc cur-index)) regs sound)))))))

(defn run-programs
  [ins]
  )
