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
    (is (= [[state-right [:right] 3]
            [state-down [:down] 3]]
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
    (is (= [[state-down [:down] 9]
            [state-left [:left] 5]]
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
    (is (= [[state-up [:up] 25]
            [state-right [:right] 27]]
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
    (is (= [[state-up [:up] 29]
            [state-left [:left] 29]]
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
    (is (= [[state-up [:up] 29]
            [state-right [:right] 31]
            [state-down [:down] 31]
            [state-left [:left] 27]]
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
    (is (= [[state-left [:left] 29]]
           (successor state-initial [] #{state-up})))))

  
(run-tests)

(time (solve (generate-state 5)))

