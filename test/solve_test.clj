(ns solve-test
  (:use [clojure.test]
        [solve]))

(deftest test-successor
  (testing "top left edge" 
    (def state-initial [0  1  2  3
                        4  5  6  7
                        8  9 10 11
                       12 13 14 15])
    (def state-right [1  0  2  3
                      4  5  6  7
                      8  9 10 11
                     12 13 14 15])
    (def state-down [4  1  2  3
                     0  5  6  7
                     8  9 10 11
                    12 13 14 15])
    (is (= [[state-right [:right]]
            [state-down [:down]]]
           (successor state-initial [] []))))
  (testing "top right edge"
    (def state-initial [1  2  3  0
                        4  5  6  7
                        8  9 10 11
                       12 13 14 15])
    (def state-down [1  2  3  7
                     4  5  6  0
                     8  9 10 11
                    12 13 14 15])
    (def state-left [1  2  0  3
                     4  5  6  7
                     8  9 10 11
                    12 13 14 15])
    (is (= [[state-down [:down]]
            [state-left [:left]]]
           (successor state-initial [] []))))
  (testing "bottom left edge"
    (def state-initial [1  2  3  4
                        5  6  7  8
                        9 10 11 12
                        0 13 14 15])
    (def state-up [1  2  3  4
                   5  6  7  8
                   0 10 11 12
                   9 13 14 15])
    (def state-right [1  2  3  4
                      5  6  7  8
                      9 10 11 12
                     13  0 14 15])
    (is (= [[state-up [:up]]
            [state-right [:right]]]
           (successor state-initial [] []))))
  (testing "bottom right edge"
    (def state-initial [1  2  3  4
                        5  6  7  8
                        9 10 11 12
                       13 14 15  0])
    (def state-up [1  2  3  4
                   5  6  7  8
                   9 10 11  0
                  13 14 15 12])
    (def state-left [1  2  3  4
                     5  6  7  8
                     9 10 11 12
                    13 14  0 15])
    (is (= [[state-up [:up]]
            [state-left [:left]]]
           (successor state-initial [] []))))
  (testing "center"
    (def state-initial [1  2  3  4
                        5  0  7  8
                        9 10 11 12
                       13 14 15  6])
    (def state-up [1  0  3  4
                   5  2  7  8
                   9 10 11 12
                  13 14 15  6])
    (def state-right [1  2  3  4
                      5  7  0  8
                      9 10 11 12
                     13 14 15  6])
    (def state-down [1  2  3  4
                     5 10  7  8
                     9  0 11 12
                    13 14 15  6])
    (def state-left [1  2  3  4
                     0  5  7  8
                     9 10 11 12
                    13 14 15  6])
    (is (= [[state-up [:up]]
            [state-right [:right]]
            [state-down [:down]]
            [state-left [:left]]]
           (successor state-initial [] []))))
  (testing "exclude state already in path"
    (def state-initial [1  2  3  4
                        5  6  7  8
                        9 10 11 12
                       13 14 15  0])
    (def state-up [1  2  3  4
                   5  6  7  8
                   9 10 11  0
                  13 14 15 12])
    (def state-left [1  2  3  4
                     5  6  7  8
                     9 10 11 12
                    13 14  0 15])
    (is (= [[state-left [:left]]]
           (successor state-initial [] #{state-up})))))

(deftest test-solution
  (is (= []
         (solve solution)))
  (is (= [:up :right :down :right :right :down :left :up :left :left :up :up]
         (solve [4 1 2 3 9 8 6 7 0 5 10 14 12 13 15 11]))))

  
(run-tests)
  
;(time (solve (generate-state 100)))
(time (solve [4 1 2 3 9 8 6 7 0 5 10 14 12 13 15 11]))

;explored 48725 states
;solved [5 2 1 7 10 4 3 0 9 13 11 15 8 12 14 6] using solution [:up :left :left :down :left :down :down :right :right :right :up :up :left :up :right :down :down :left :down :left :up :up :left :up :right :right :down :down :left :up :left :up] in 32 steps
;"Elapsed time: 6099.713 msecs"
