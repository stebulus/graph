(ns gmwbot.analyze-test
  (:use clojure.test gmwbot.analyze))
(deftest vmap
  (is (= {0 [1 1 0] 1 [] 2 []}
         (vertex-map #(mod % 3) {0 [1 4 3] 2 []}))))
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
    (is (= (scc (merge-with into four {4 [1]}))
           #{#{1 2 3 4}}))
    (is (= (scc (merge-with into four {4 [2]}))
           #{#{1} #{2 3 4}}))
    (is (= (scc (merge-with into four {4 [3]}))
           #{#{1} #{2 3 4}}))))
(deftest condensation
  (is (= (condense {:top [:left :right] :left [:top] :right []})
         {#{:top :left} [#{:right}] #{:right} []}))
  (is (= (condense {:top [:left :right] :left [] :right [:top]})
         {#{:top :right} [#{:left}] #{:left} []}))
  (is (= (condense {:top [:left :right] :left [:top] :right [:top]})
         {#{:top :left :right} []}))
  (is (= (condense {:top [:left :right] :left [:top] :right [:left]})
         {#{:top :left :right} []}))
  (is (= (condense {:top [:left :right] :left [] :right [:top :left]})
         {#{:top :right} [#{:left}] #{:left} []}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (condense (merge-with into four {4 [1]}))
           {#{1 2 3 4} []}))
    (is (= (condense (merge-with into four {4 [2]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))
    (is (= (condense (merge-with into four {4 [3]}))
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
(deftest followdep
  (is (= {:a [] :b [] :c [:a]}
         (follow-deps {:a [[:b :c]]
                       :b [["-"]]
                       :c [["+"]]})))
  (is (= {:a [] :b [:a] :c [:a]}
         (follow-deps {:a [[:b :c]]
                       :b [["-"]]
                       :c [["+"]
                           []]}))))
(deftest follows
  (let [grammar {:exprs [[:expr :exprs]
                         []]
                 :expr [["(" :exprs ")"]]}
        nullable? (make-nullable? grammar)
        firsts (make-first-set grammar nullable?)]
    (is (= (follow-sets grammar nullable? firsts)
           {:exprs #{")"}
            :expr #{"(" ")"}})))
  (let [grammar {:sum [[:term :more-terms]]
                 :more-terms [["+" :sum]
                              []]
                 :term [[:factor :more-factors]]
                 :more-factors [["*" :term]
                                []]
                 :factor [[:number]
                          ["(" :sum ")"]]}
        nullable? (make-nullable? grammar)
        firsts (make-first-set grammar nullable?)]
    (is (= (follow-sets grammar nullable? firsts)
           {:sum #{")"}
            :more-terms #{")"}
            :term #{"+" ")"}
            :more-factors #{"+" ")"}
            :factor #{"*" "+" ")"}}))))
