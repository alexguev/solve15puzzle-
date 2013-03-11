(ns solve)

(defn action-block-index [block-index action]
  (let [col (mod block-index 4)
        row (quot block-index 4)
        action-col (+ col (action {:up 0 :right 1 :down 0 :left -1}))
        action-row (+ row (action {:up -1 :right 0 :down 1 :left 0}))]
    (when (and (>= action-col 0) (<= action-col 3) (>= action-row 0) (<= action-row 3))
      (+ (* 4 action-row) action-col))))

(defn play-action [state action]
  (let [[empty-block-index] (keep-indexed #(if (= %2 0) %1) state)
        action-block-index (action-block-index empty-block-index action)]
    (when-not (nil? action-block-index) 
      (assoc state 
        empty-block-index (nth state action-block-index) 
        action-block-index 0))))

(defn step-cost [state]
  (apply + (keep-indexed #(if (or (nil? %2) (= (+ %1 1) %2)) 0 1) 
                         state)))

(defn explored-states [frontier]
  (into (set (keys frontier))
          (mapcat (fn [{:keys [path]}] path) (vals frontier))))

(defn successor-fn [frontier]
  (->> (vec frontier)
       (mapcat (fn [[state {:keys [cost path]}]]
                 [{:path path :state state :cost cost :action :up} 
                  {:path path :state state :cost cost :action :right}
                  {:path path :state state :cost cost :action :down}
                  {:path path :state state :cost cost :action :left}]))
       (map (fn [{:keys [path cost state action]}]
              (when-let [next-state (play-action state action)]
                {:path path :state state :cost cost :action action :next-state next-state})))
       (filter (comp not nil?))
       (filter (fn [{:keys [path next-state]}] (not-any? #(= next-state %) 
                                                         (explored-states frontier))))
       (seq)))

(def state-solved [1  2  3  4
                   5  6  7  8 
                   9 10 11 12
                  13 14 15  0])

(defn solved [state]
  (= state-solved state))

(defn min-cost-state [frontier]
  (apply min-key (fn [[state {:keys [cost path]}]] (+ (count path) cost)) (vec frontier)))

(defn a* 
  "solves the 15 Puzzle using the A* algorithm."
  [frontier]
  (let [[min-state {min-path :path}] (min-cost-state frontier)]
    (if (solved min-state)
      (conj min-path min-state)
      (when-let [successors (successor-fn frontier)]
        (recur (into (dissoc frontier min-state)
                     (map (fn [{:keys [path state cost action next-state]}] 
                            {next-state {:cost (+ cost (step-cost next-state))
                                         :path (conj path state action)}}) 
                          successors)))))))

(defn solve 
  [state]
  (a* {state {:cost (step-cost state) :path []}}))

(defn generate-state [n]
  (nth (iterate (fn [s] (->> (map (partial play-action s) [:up :right :down :left])
                             (filter (comp not nil?))
                             (rand-nth)))
                state-solved)
       n))