(ns gmwbot.test
  (:use clojure.test gmwbot.analyze))
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
                      (combiner +))]
      (is (= (f {} :c) {:c 2}))
      (is (= (f {} :d) {:d 4}))
      (is (= (f {} :b) {:b 7 :c 2 :d 4}))
      (is (= (f {} :a) {:a 12 :b 7 :c 2 :d 4})))  ; note :c 2 counted twice
    (let [f (recurser #(list (first (lgraph %)))
                      #(second (lgraph %))
                      (combiner concat))]
      (is (= (f {} :c) {:c [2]}))
      (is (= (f {} :d) {:d [4]}))
      (is (= (f {} :b) {:b [1 2 4] :c [2] :d [4]}))
      (is (= (f {} :a) {:a [3 1 2 4 2] :b [1 2 4] :c [2] :d [4]}))))
  (let [graph {:a [:b :c]
               :b [:a :c]
               :c []}
        f (recurser (fn [_] 1) graph (combiner +))]
    (is (thrown-with-msg? IllegalArgumentException #"recursion :a"
                 (f {} :a)))
    (is (thrown-with-msg? IllegalArgumentException #"recursion :b"
                 (f {} :b)))
    (is (= (f {} :c) {:c 1})))
  (let [graph {:a [all [:b :c]]
               :b [(constantly true) []]
               :c [all [:a]]}
        f (recurser #(first (graph %))
                    #(second (graph %))
                    (fn [f & args] (apply f args)))]
    (is (thrown-with-msg? IllegalArgumentException #"recursion :a"
          (f {} :a))))
  (let [graph {:a [all [:b :c]]
               :b [(constantly false) []]
               :c [all [:a]]}
        f (recurser #(first (graph %))
                    #(second (graph %))
                    (fn [f & args] (apply f args)))]
    (is (= (f {} :a) {:a false :b false})))
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
    (is (thrown? AssertionError
                 (first-sets grammar (make-nullable? grammar)))))
  (let [grammar {:s [[:f]
                     ["(" :s "+" :f ")"]
                     []]
                 :f [["a"]]
                 :g [["b"]
                     []]}]
    (is (= (first-sets grammar (make-nullable? grammar))
           {:s #{"a" "("}
            :f #{"a"}
            :g #{"b"}}))))
