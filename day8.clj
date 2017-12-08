(defn clean-register-data
  [f]
  (->> f
      slurp
      clojure.string/split-lines
      (map #(clojure.string/split % #" "))))

(defn compile-register-program
  [program]
  (reduce
   (fn [state [reg op val _ ifreg ifop ifval]]
     (let [regkey (keyword reg)
           ifregkey (keyword ifreg)
           add-key-to-state (fn [key state] (if (key state)
                                              state
                                              (assoc state key 0)))
           newstate (add-key-to-state ifregkey (add-key-to-state regkey state))
           ifstateval (ifregkey newstate)
           val (Integer/parseInt val)
           ifval (Integer/parseInt ifval)
           updated-state (if (cond
                               (= ifop ">") (> ifstateval ifval)
                               (= ifop "<") (< ifstateval ifval)
                               (= ifop ">=") (>= ifstateval ifval)
                               (= ifop "==") (== ifstateval ifval)
                               (= ifop "<=") (<= ifstateval ifval)
                               (= ifop "!=") (not= ifstateval ifval))
                           (update newstate regkey #(cond
                                                      (= op "inc") (+ % val)
                                                      (= op "dec") (- % val)))
                           newstate)]
       (assoc updated-state :highest (biggest-value updated-state))
       ))
   {:highest 0} program))

(defn biggest-value
  [m]
  (apply max (vals m)))
