(ns gmwbot.analyze-test
  (:use clojure.test gmwbot.analyze))
(deftest sccs
  (is (= (scc {:top [:left :right] :left [:top] :right []})
         #{#{:top :left} #{:right}}))
  (is (= (scc {:top [:left :right] :left [] :right [:top]})
         #{#{:top :right} #{:left}}))
  (is (= (scc {:top [:left :right] :left [:top] :right [:top]})
         #{#{:top :left :right}}))
  (is (= (scc {:top [:left :right] :left [:top] :right [:left]})
         #{#{:top :left :right}}))
  (is (= (scc {:top [:left :right] :left [] :right [:top :left]})
         #{#{:top :right} #{:left}}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (scc (merge-with concat four {4 [1]}))
           #{#{1 2 3 4}}))
    (is (= (scc (merge-with concat four {4 [2]}))
           #{#{1} #{2 3 4}}))
    (is (= (scc (merge-with concat four {4 [3]}))
           #{#{1} #{2 3 4}}))))
(deftest condensation
  (is (= (condense {:top [:left :right] :left [:top] :right []})
         {#{:top :left} [#{:right}]}))
  (is (= (condense {:top [:left :right] :left [] :right [:top]})
         {#{:top :right} [#{:left}]}))
  (is (= (condense {:top [:left :right] :left [:top] :right [:top]})
         {#{:top :left :right} []}))
  (is (= (condense {:top [:left :right] :left [:top] :right [:left]})
         {#{:top :left :right} []}))
  (is (= (condense {:top [:left :right] :left [] :right [:top :left]})
         {#{:top :right} [#{:left}]}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (condense (merge-with concat four {4 [1]}))
           {#{1 2 3 4} []}))
    (is (= (condense (merge-with concat four {4 [2]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))
    (is (= (condense (merge-with concat four {4 [3]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))))
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
(deftest firsts
  (is (thrown? AssertionError
               (first-sets {:a [[:b :c]
                                []]
                            :b [["terminal"]
                                []]
                            :c [[:a]
                                []]})))
  (is (= (first-sets {:s [[:f]
                          ["(" :s "+" :f ")"]
                          []]
                      :f [["a"]]
                      :g [["b"]
                          []]})
         {:s #{"a" "("}
          :f #{"a"}
          :g #{"b"}})))
