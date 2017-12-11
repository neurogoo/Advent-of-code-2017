(defn parse-hash-input
  [f]
  (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trim-newline (slurp f)) #",")))

(defn parse-char-hash-input
  [f]
  (concat (->> (slurp f)
               clojure.string/trim-newline
               seq
               (map #(int %)))
          (list 17 31 73 47 23)))

(defn cycle-take
  [hashkey start len]
  (take len (drop start (cycle hashkey))))

(defn cycle-update
  [input start new-seq]
  (let [input-size (count input)]
    (loop [i (range (count new-seq))
           n new-seq
           updated-input input]
      (if (not (seq n))
        updated-input
        (recur (rest i)
               (rest n)
               (assoc updated-input (mod (+ start (first i)) input-size) (first n)))))))

(defn process-hash
  [input n]
  (let [input-size (count input)]
      (loop [pos 0
             skip-size 0
             key (vec (range 0 256))
             i (flatten (repeat n input))]
        (if (not (seq i))
          key
          (let [new-seq (reverse (cycle-take key pos (first i)))]
            (recur (mod (+ pos (first i) skip-size) (count key))
                   (inc skip-size)
                   (cycle-update key pos new-seq)
                   (rest i)))))))
(defn form-dense-hash
  [sparse-hash]
  (map #(apply bit-xor %) (partition 16 sparse-hash)))

(defn to-hex-hash
  [dense]
  (let [pad (fn [s] (if (> (count s) 1)
                      s
                      (str "0" s)))]
    (apply str (map #(pad (Integer/toHexString %)) dense))))
