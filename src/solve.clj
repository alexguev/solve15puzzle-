(ns solve)


;       (map (fn [{:keys [path cost state action next-state]}] 
;              {:path path :cost cost :state state :action :right :next-state next-state :step-cost (calculate-step-cost next-state)}))

(defn best [successors]
  (first successors))

(defn calculate-action-block-index [block-index action]
  (let [col (mod block-index 4)
        row (quot block-index 4)
        action-col (+ col (action {:up 0 :right 1 :down 0 :left -1}))
        action-row (+ row (action {:up -1 :right 0 :down 1 :left 0}))]
    (when (and (>= action-col 0) (<= action-col 3) (>= action-row 0) (<= action-row 3))
      (+ (* 4 action-row) action-col))))

(defn play-action [action state]
  (let [[empty-block-index] (keep-indexed #(if (= %2 0) %1) state)
        action-block-index (calculate-action-block-index empty-block-index action)]
    (when-not (nil? action-block-index) 
      (assoc state 
        empty-block-index (nth state action-block-index) 
        action-block-index 0))))

(defn calculate-step-cost [state]
  (apply + 
         (keep-indexed #(if (or (nil? %2) (= (+ %1 1) %2)) 
                          0 
                          1) 
                       state)))

;move calculation of step cost out of successor (it should be in best)
(defn successor [frontier]
  (->> (vec frontier)
       (mapcat (fn [[state {:keys [cost path]}]]
                 [{:path path :cost cost :state state :action :up} 
                  {:path path :cost cost :state state :action :right}
                  {:path path :cost cost :state state :action :down}
                  {:path path :cost cost :state state :action :left}]))
       (map (fn [{:keys [path cost state action]}]
              (when-let [next-state (play-action action state)]
                {:path path :cost cost :state state :action action :next-state next-state})))
       (filter (comp not nil?))
       (filter (fn [{:keys [path next-state]}] (not-any? #(= next-state %) path)))))

(def state-solved [1  2  3  4
                   5  6  7  8
                   9 10 11 12
                  13 14 15 nil])

(defn solved [state]
  (= state-solved state))

(defn a* 
  "solves the 15 Puzzle using the A* algorithm."
  [frontier]
  (let [successors (successor frontier)]
    (when (seq successors)
      (let [[path cost state action next-state step-cost] (best successors)]
        (if (solved next-state)
          (conj path [action next-state])
          (recur (assoc frontier next-state {:cost (+ cost step-cost) :path (conj path state action)})))))))

(defn solve 
  [state]
  (if (solved state) ; move this to a*
    [state]
    (a* {state {:cost 0 :path []}})))









