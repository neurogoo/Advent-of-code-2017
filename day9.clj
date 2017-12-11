(def start-map {:context :group
                :level 0
                :content []})

(defn group-score
  [group-map]
  (reduce (fn [s group]
            (+ s (:level group) (group-score group)))
          0 (:content group-map)))

(defn construct-stream-map
  [start-map start-stream char-count]
  (loop [m start-map
         [next & r] start-stream
         c char-count]
    (if (or (nil? next) (and (= (:context m) :group) (= \} next)))
      [m r c]
      (let [[new-map new-stream new-count]
            (if (= (:context m) :garbage)
              (case next
                \! [m (rest r) c]
                \> [(assoc m :context :group) r c]
                [m r (inc c)])
              (case next
                \{ (let [[inner-map r c] (construct-stream-map {:context :group
                                                                :level (inc (:level m))
                                                                :content []} r c)]
                     [(update m :content #(conj % inner-map)) r c])
                \< [(assoc m :context :garbage) r c]
                [m r c]))]
        (recur new-map new-stream new-count)))))
