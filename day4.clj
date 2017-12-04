(defn clean-passphrase-data
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map (fn [l] (clojure.string/split l #" ")))))

(defn check-duplicates
  [wordlist]
  (reduce +
          (map (fn [l]
                 (if (apply distinct? l)
                   1
                   0)) wordlist)))

(defn check-anagrams
  [wordlist]
  (reduce +
          (map (fn [l]
                 (if (apply distinct? (map sort l))
                   1
                   0)) wordlist)))
