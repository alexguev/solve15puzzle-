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


; use manhattan distance
(defn step-cost [state]
  (apply + (keep-indexed #(if (or (= 0 %2) (= (+ %1 1) %2)) 0 1) 
                         state)))

(defn successor
  [state path explored]
  (->> (map (fn [action] (when-let [next-state (play-action state action)] 
                           [action next-state])) 
            [:up :right :down :left])  
       (filter (comp not nil?))
       (remove (fn [[action next-state]] (contains? explored next-state)))
       (map (fn [[action next-state]] [next-state (conj path action)]))
       (seq)))

(def state-solved [1  2  3  4
                   5  6  7  8 
                   9 10 11 12
                  13 14 15  0])

(defn solved [state]
  (= state-solved state))

(defn best-state [fringe] 
  (apply min-key (fn [[state path]] (+ (count path) (step-cost state))) fringe))

(defn fringe-sort-fn [[s1 p1] [s2 p2]]
  (compare (+ (step-cost s1) (count p1))
           (+ (step-cost s2) (count p2))))

; tbd: use transient fringe
(defn a* 
  "solves the 15 Puzzle using the A* algorithm.
    'state' is the initial state"
  [state]
  (loop [fringe {state []}
         explored (transient #{})]
    (let [[best-state best-path] (best-state fringe)]
      (if (solved best-state)
        best-path
        (let [new-explored (conj! explored best-state)]
          (when-let [successors (successor best-state best-path new-explored)]
            (let [new-fringe (into (dissoc fringe best-state) 
                                   successors)]
              (recur new-fringe new-explored))))))))

; tbd: generalize to n puzzle
(defn solve
  [state]
  (println (format "solving: %s" state))
  (let [solution (a* state)]
    (println (format "solved in %s steps" (count solution)))
    solution))

(defn generate-state [n]
  (nth (iterate (fn [s] (->> (map (partial play-action s) [:up :right :down :left])
                             (filter (comp not nil?))
                             (rand-nth)))
                state-solved)
       n))

(def s1 [1 5 2 3 4 0 6 10 8 9 11 7 12 13 14 15])

(def s2 [2 3 0 4 1 6 8 12 5 9 10 7 13 14 11 15])

(solve (generate-state 20))

(persistent! (reduce conj! (transient [1]) [2 3 4 5]))

;(solve s)
