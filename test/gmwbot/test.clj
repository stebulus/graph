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
