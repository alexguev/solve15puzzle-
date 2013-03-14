(ns solve)

(def n 4) ; the puzzle size, where the total number of sliding blocks is n*n-1.

(defn to-col [index] (mod index n))
(defn to-row [index] (quot index n))
(defn to-index [col row] (+ (* n row) col))

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
  (+ (reduce + (for [idx (range (* n n))] 
                 (manhattan-distance [(to-col idx) (to-row idx)] [(to-col (nth state idx)) (to-row (nth state idx))])))
     (count path)))

(defn successor
  [state path explored]
  (->> (map (fn [action] (when-let [new-state (play-action state action)] [action new-state])) 
            [:up :right :down :left])  
       (filter (comp not nil?))
       (remove (fn [[action new-state]] (contains? explored new-state)))
       (map (fn [[action new-state]] [new-state (conj path action)]))))

(def solution (vec (range (* n n))))

(defn solved [state]
  (= solution state))

(defn best-state [fringe] 
  (first fringe))

(defn fringe-sort-fn [[s1 p1 c1] [s2 p2 c2]]
  (let [result (compare c1 c2)]
    (if (zero? result)
      (compare s1 s2)
      result)))

(defn pfargs
  [f coll]
  (let [p (promise)
        futs (for [args coll] (future (let [r (apply f args)] (deliver p r))))]
    (let [v @p] 
      (doseq [fut futs] (when-not (future-done? fut) (future-cancel fut)))
      v)))

(defn a* 
  "solves the 15 Puzzle using the A* algorithm.
    's' is the initial state
    'f' the step cost function f(s, p), where 's' is a state and 'p' is the path taken to get to 's'"
  [s f]
  (loop [fringe (sorted-set-by fringe-sort-fn [s [] (f s [])]) 
         explored #{}]
    (println (format "explored %s states" (count explored)))
    (when (seq fringe)
      (let [[best-state best-path _ :as best-node] (best-state fringe)]
        (if (solved best-state)
          best-path
          (let [new-explored (conj explored best-state)
                successors (successor best-state best-path new-explored)
                new-fringe (into (disj fringe best-node) 
                                 (map (fn [[s p]] [s p (f s p)]) successors))]
            (recur new-fringe new-explored)))))))

(defn solve
  [s]
  (let [solution (a* s step-cost)]
    (println (format "solved %s using solution %s in %s steps" s solution (count solution)))
    solution))

(defn generate-state [steps]
  (nth (iterate (fn [s] (->> (map (partial play-action s) [:up :right :down :left])
                             (filter (comp not nil?))
                             (rand-nth)))
                solution)
       steps))

