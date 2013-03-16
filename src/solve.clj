(ns solve)

(def n 4) ; the puzzle size, where the total number of sliding blocks is n*n-1.

(defn coords [index]
  [(mod index n) (quot index n)])

(defn index [x y] (+ (* n y) x))

(defn action-block-index [block-index action]
  (let [[x y] (coords block-index)
        new-x (+ x (action {:up 0 :right 1 :down 0 :left -1}))
        new-y (+ y (action {:up -1 :right 0 :down 1 :left 0}))]
    (when (and (>= new-x 0) (<= new-x 3) (>= new-y 0) (<= new-y 3))
      (index new-x new-y))))

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

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn step-cost [state path]
  (+ (reduce + (for [idx (range (* n n))] 
                 (manhattan-distance (coords idx) (coords (nth state idx)))))
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

(defn a* 
  "solves the 15 Puzzle using the A* algorithm.
    's' is the initial state
    'f' the step cost function f(s, p), where 's' is a state and 'p' is the path taken to get to 's'"
  [s f]
  (loop [fringe (sorted-set-by fringe-sort-fn [s [] (f s [])]) 
         explored #{}]
    (when (seq fringe)
      (let [[best-state best-path _ :as best-node] (best-state fringe)]
        (if (solved best-state)
          (do (println (format "explored %s states" (count explored)))
              best-path)
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

(defn gen-state [steps]
  (nth (iterate (fn [s] (->> (map (partial play-action s) [:up :right :down :left])
                             (filter (comp not nil?))
                             (rand-nth)))
                solution)
       steps))

(defn pfargs
  [f coll]
  (let [p (promise)
        futs (for [args coll] (future (let [r (apply f args)] (deliver p r))))]
    (doall futs)
    (let [v @p] 
      (doseq [fut futs] (when-not (future-done? fut) (future-cancel fut)))
      v)))
