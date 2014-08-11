(ns amotlpaa.analyze-test
  (:use clojure.test amotlpaa.analyze))
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
