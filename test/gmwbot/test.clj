(ns gmwbot.test
  (:use clojure.test gmwbot.analyze))
(deftest reach
  ;; edge case
  (is (= (reachable {} [:a :b]) '()))
  ;; simple traversal
  (is (= (reachable {:a [1 [:b]]
                     :b [1 []]}
                    [:a])
         [:a :b]))
  (is (= (reachable {:a [1 [:b :c]]
                     :b [1 [:d]]
                     :c [1 [:d]]
                     :d [1 []]}
                    [:a])
         [:a :b :c :d]))
  ;; thresholds
  (is (= (reachable {:a [1 [:b :c]]
                     :b [1 [:d]]
                     :c [1 [:d]]
                     :d [2 []]}
                    [:a])
         [:a :b :c :d]))
  (is (= (reachable {:a [1 [:b :c]]
                     :b [1 [:d]]
                     :c [1 [:d]]
                     :d [3 []]}
                    [:a])
         [:a :b :c]))
  (is (= (reachable {:a [1 [:b :c]]
                     :b [2 [:d]]
                     :c [1 [:d]]
                     :d [2 []]}
                    [:a])
         [:a :c]))
  (is (= (reachable {:a [1 [:b :c]]
                     :b [2 [:d]]
                     :c [1 [:d]]
                     :d [1 []]}
                    [:a])
         [:a :c :d]))
  ;; loops
  (is (= (reachable {:a [1 [:a]]}
                    [:a])
         [:a]))
  (is (= (reachable {:a [1 [:b]]
                     :b [1 [:a]]}
                    [:a])
         [:a :b]))
  ;; multiple edges
  (is (= (reachable {:a [1 [:b :b]]
                     :b [2 []]}
                    [:a])
         [:a :b])))
(deftest nullability
  (is (= (nullables {:zero-or-more [[:x :zero-or-more]
                                    []]})
         [:zero-or-more]))
  (is (= (nullables {:one-or-more [[:x :zero-or-more]]
                     :zero-or-more [[:x :zero-or-more]
                                    []]})
         [:zero-or-more]))
  (is (= (set (nullables {:a [[:b :c]]
                          :b [[:z]
                              []]
                          :c [[:z]
                              []]}))
         #{:a :b :c}))
  (is (= (nullables {:a [[:b :c]]
                     :b [[:z]
                         []]
                     :c [[:z]]})
         [:b]))
  (is (= (nullables {:a [[:b :c]]
                     :b [[:z]]
                     :c [[:z]
                         []]})
         [:c])))
(deftest recursive-traversal
  (let [lgraph {:a [3 [:b :c]]
                :b [1 [:c :d]]
                :c [2 []]
                :d [4 []]}]
    (let [f (recurser #(first (lgraph %))
                      #(second (lgraph %))
                      +)]
      (is (= (f {} :c) {:c 2}))
      (is (= (f {} :d) {:d 4}))
      (is (= (f {} :b) {:b 7 :c 2 :d 4}))
      (is (= (f {} :a) {:a 12 :b 7 :c 2 :d 4})))  ; note :c 2 counted twice
    (let [f (recurser #(list (first (lgraph %)))
                      #(second (lgraph %))
                      concat)]
      (is (= (f {} :c) {:c [2]}))
      (is (= (f {} :d) {:d [4]}))
      (is (= (f {} :b) {:b [1 2 4] :c [2] :d [4]}))
      (is (= (f {} :a) {:a [3 1 2 4 2] :b [1 2 4] :c [2] :d [4]}))))
  (let [graph {:a [:b :c]
               :b [:a :c]
               :c []}
        f (recurser (fn [_] 1) graph +)]
    (is (thrown-with-msg? IllegalArgumentException #"recursion :a"
                 (f {} :a)))
    (is (thrown-with-msg? IllegalArgumentException #"recursion :b"
                 (f {} :b)))
    (is (= (f {} :c) {:c 1})))
  (let [graph {:a [+ [:b :c]]
               :b [* [:d :f]]
               :c [* [:d :e]]
               :d [(constantly 3) []]
               :e [(constantly 4) []]
               :f [(constantly 2) []]}
        f (recurser #(first (graph %))
                    #(second (graph %))
                    (fn [f & args] (apply f args)))]
    (is (= (f {} :a)
           {:a 18
            :b 6
            :c 12
            :d 3
            :e 4
            :f 2}))))
(deftest firsts
  (let [grammar {:a [[:b :c]
                     []]
                 :b [["terminal"]
                     []]
                 :c [[:a]
                     []]}]
    (is (thrown? IllegalArgumentException
                 (first-sets grammar (set (nullables grammar))))))
  (let [grammar {:s [[:f]
                     ["(" :s "+" :f ")"]
                     []]
                 :f [["a"]]
                 :g [["b"]
                     []]}
        nullable? (set (nullables grammar))
        first-set (first-sets grammar nullable?)]
    (is (= (first-sets grammar nullable?)
           {[:s] #{"a" "("}
            [:s 0] #{"a"}
            [:s 1] #{"("}
            [:s 2] #{}
            [:f] #{"a"}
            [:f 0] #{"a"}
            [:g] #{"b"}
            [:g 0] #{"b"}
            [:g 1] #{}
            ["a"] #{"a"}
            ["b"] #{"b"}
            ["("] #{"("}
            }))))
