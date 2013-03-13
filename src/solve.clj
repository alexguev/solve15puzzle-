(ns solve)

(defn to-col [index] (mod index 4))
(defn to-row [index] (quot index 4))
(defn to-index [col row] (+ (* 4 row) col))

(defn action-block-index [block-index action]
  (let [col (to-col block-index)
        row (to-row block-index)
        action-col (+ col (action {:up 0 :right 1 :down 0 :left -1}))
        action-row (+ row (action {:up -1 :right 0 :down 1 :left 0}))]
    (when (and (>= action-col 0) (<= action-col 3) (>= action-row 0) (<= action-row 3))
      (to-index action-col action-row))))

(defn play-action [state action]
  (let [[empty-block-index] (keep-indexed #(if (= %2 0) %1) state)
        action-block-index (action-block-index empty-block-index action)]
    (when-not (nil? action-block-index) 
      (assoc state 
        empty-block-index (nth state action-block-index) 
        action-block-index 0))))

;(defn step-cost [state path]
;  (+ (apply + (keep-indexed #(if (= %1 %2) 0 1) state))
;     (count path)))

(defn abs [n]
  (if (< 0 n) n (- n)))

(defn manhattan-distance [[col1 row1] [col2 row2]]
  (+ (abs (- col1 col2))
     (abs (- row1 row2))))

(defn step-cost [state path]
  (+ (reduce + (for [idx (range 16)] 
                 (manhattan-distance [(to-col idx) (to-row idx)] [(to-col (nth state idx)) (to-row (nth state idx))])))
     (count path)))

(defn successor
  [state path explored]
  (->> (map (fn [action] 
              (when-let [new-state (play-action state action)] [action new-state])) 
            [:up :right :down :left])  
       (filter (comp not nil?))
       (remove (fn [[action new-state]] (contains? explored new-state)))
       (map (fn [[action new-state]] 
              (let [new-path (conj path action)
                    new-cost (step-cost new-state new-path)]
                [new-state new-path new-cost])))))

(def state-solved [0  1  2  3
                   4  5  6  7 
                   8  9 10 11
                  12 13 14 15])

(defn solved [state]
  (= state-solved state))

(defn best-state [fringe] 
  (first fringe))

(defn fringe-sort-fn [[s1 p1 c1] [s2 p2 c2]]
  (let [result (compare c1 c2)]
    (if (zero? result)
      (compare s1 s2)
      result)))

(defn a* 
  "solves the 15 Puzzle using the A* algorithm.
    'state' is the initial state"
  [state]
  (loop [fringe (sorted-set-by fringe-sort-fn [state [] (step-cost state [])]) 
         explored (transient #{})]
    (println (format "explored %s states" (count explored)))
    (when (seq fringe)
      (let [[best-state best-path _ :as best-node] (best-state fringe)]
        (if (solved best-state)
          best-path
          (let [new-explored (conj! explored best-state)
                successors (successor best-state best-path new-explored)
                new-fringe (into (disj fringe best-node) successors)]
            (recur new-fringe new-explored)))))))

; tbd: generalize to n puzzle
(defn solve
  [state]
  (println (format "solving: %s" state))
  (let [solution (a* state)]
    (println (format "solved %s in %s steps" state (count solution)))
    solution))

(defn generate-state [n]
  (nth (iterate (fn [s] (->> (map (partial play-action s) [:up :right :down :left])
                             (filter (comp not nil?))
                             (rand-nth)))
                state-solved)
       n))
