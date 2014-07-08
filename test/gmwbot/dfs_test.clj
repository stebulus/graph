(ns gmwbot.dfs-test
  (:use clojure.test
        gmwbot.dfs))
(deftest dfs-down
  (as-> (dfs {:a [:b] :b [:c]} :a)
        search
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [nil :a]))
          (down search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:a :b]))
          (down search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:b :c]))
          (down search))
        (do
          (is (failed? search))
          (is (= (last-edge search) [:b :c])))))
(deftest dfs-across
  (as-> (dfs {:a [:b :c :d]} :a)
        search
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [nil :a]))
          (down search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:a :b]))
          (across search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:a :c]))
          (across search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:a :d]))
          (across search))
        (do
          (is (failed? search))
          (is (= (last-edge search) [:a :d])))))
(deftest dfs-up
  (as-> (dfs {:a [:b] :b [:c]} :a)
        search
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [nil :a]))
          (down search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:a :b]))
          (down search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:b :c]))
          (up search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [:a :b]))
          (up search))
        (do
          (is (not (failed? search)))
          (is (= (last-edge search) [nil :a]))
          (up search))
        (do
          (is (failed? search))
          (is (= (last-edge search) [nil :a])))))
