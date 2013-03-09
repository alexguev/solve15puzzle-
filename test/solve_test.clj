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
    (is (= [{:path [] :cost 0 :state state-initial :action :right :next-state state-right}
            {:path [] :cost 0 :state state-initial :action :down :next-state state-down}]
           (successor {state-initial {:cost 0 :path []}}))))
  (testing "top right edge"
    (def state-initial [1  2  3  0
                        4  5  6  7
                        8  9 10 11
                       12 13 14 15])
    (def state-left [1  2  0  3
                     4  5  6  7
                     8  9 10 11
                    12 13 14 15])
    (def state-down [1  2  3  7
                     4  5  6  0
                     8  9 10 11
                    12 13 14 15])
    (is (= [{:path [] :cost 0 :state state-initial :action :down :next-state state-down}
            {:path [] :cost 0 :state state-initial :action :left :next-state state-left}]
           (successor {state-initial {:cost 0 :path []}})))))

(run-tests 'solve-test)
