(ns gmwbot.analyze-test
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
