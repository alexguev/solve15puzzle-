(ns solve-test
  (:use [clojure.test]
        [solve]))

(deftest test-successor-fn
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
    (is (= [{:path [] :cost 0 :state state-initial :action :right :next-state state-right}
            {:path [] :cost 0 :state state-initial :action :down :next-state state-down}]
           (successor-fn {state-initial {:cost 0 :path []}}))))
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
    (is (= [{:path [] :cost 0 :state state-initial :action :down :next-state state-down}
            {:path [] :cost 0 :state state-initial :action :left :next-state state-left}]
           (successor-fn {state-initial {:cost 0 :path []}}))))
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
    (is (= [{:path [] :cost 0 :state state-initial :action :up :next-state state-up}
            {:path [] :cost 0 :state state-initial :action :right :next-state state-right}]
           (successor-fn {state-initial {:cost 0 :path []}}))))
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
    (is (= [{:path [] :cost 0 :state state-initial :action :up :next-state state-up}
            {:path [] :cost 0 :state state-initial :action :left :next-state state-left}]
           (successor-fn {state-initial {:cost 0 :path []}}))))
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
    (is (= [{:path [] :cost 0 :state state-initial :action :up :next-state state-up}
            {:path [] :cost 0 :state state-initial :action :right :next-state state-right}
            {:path [] :cost 0 :state state-initial :action :down :next-state state-down}
            {:path [] :cost 0 :state state-initial :action :left :next-state state-left}]
           (successor-fn {state-initial {:cost 0 :path []}}))))  
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
    (is (= [{:path [state-up :down] :cost 1 :state state-initial :action :left :next-state state-left}]
           (successor-fn {state-initial {:cost 1 :path [state-up :down]}})))))

(deftest test-min-cost-state
  (testing "sigle state/path"
    (is (= ["state" {:cost 1 :path []}]
           (min-cost-state {"state" {:cost 1 :path []}}))))
  (testing "many states/paths"
    (is (= ["state-1" {:cost 1 :path []}]
           (min-cost-state {"state-1" {:cost 1 :path []}
                            "state-2" {:cost 2 :path []}})))))

(run-tests 'solve-test)

;(time (solve (generate-state 40)))

