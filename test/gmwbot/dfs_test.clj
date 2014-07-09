(ns gmwbot.dfs-test
  (:use clojure.test)
  (:require [gmwbot.dfs :as df]))

(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([search-sym edge]
    `(do
       (if-let [edge# ~edge]
         (is (= edge# (df/last-edge ~search-sym)))
         (is (nil? ~search-sym)))))
  ([search-sym edge form]
    (concat (test-traverse-clause search-sym edge)
            `((~(first form) ~search-sym ~@(rest form))))))
(defmacro test-traverse [dfs & forms]
  "Test a sequence of maneuvers in a depth-first search.  The first
  argument is an expression evaluating to an implementation of
  gmwbot.dfs/DepthFirstSearch.  The remaining forms describe a sequence
  of tests, alternating between the edge expected from (last-edge dfs)
  and a form into which dfs will be threaded as the first argument;
  the value of the resulting form will be used as the dfs for the
  subsequent forms.  For example,
    (require '[gmwbot.dfs :as df])
    (test-traverse
      (df/dfs {:a [:b]} :a)
      [nil :a]
      (df/down)
      [:a :b]
      (df/down)
      nil)
  verifies the initial state, moves down, verifies the resulting state,
  tries to move down again, and verifies that the second move failed."
  (let [search-sym (gensym 'search_)]
    `(as-> ~dfs
           ~search-sym
           ~@(map #(apply test-traverse-clause search-sym %)
                  (partition-all 2 forms)))))

(deftest down
  (test-traverse
    (df/dfs {:a [:b] :b [:c]} :a)
    [nil :a]
    (df/down)
    [:a :b]
    (df/down)
    [:b :c]
    (df/down)
    nil))
(deftest across
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    [nil :a]
    (df/down)
    [:a :b]
    (df/across)
    [:a :c]
    (df/across)
    [:a :d]
    (df/across)
    nil))
(deftest up
  (test-traverse
    (df/dfs {:a [:b] :b [:c]} :a)
    [nil :a]
    (df/down)
    [:a :b]
    (df/down)
    [:b :c]
    (df/up)
    [:a :b]
    (df/up)
    [nil :a]
    (df/up)
    nil))

(deftest scan-across
  (test-traverse
    (df/dfs {:a [:b :c :d :e]} :a)
    [nil :a]
    (df/down)
    [:a :b]
    (df/scan-across #(= :b %))
    [:a :b]
    (df/scan-across #(= :d %))
    [:a :d]
    (df/scan-across #(= :f %))
    nil))
(deftest scan-children
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    [nil :a]
    (df/scan-children #(= :c %))
    [:a :c])
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    [nil :a]
    (df/scan-children #(= :z %))
    nil))

(deftest prune-seen-parent
  (let [search (df/dfs {:a [:b] :b [:a]} :a)]
    (test-traverse
      search
      [nil :a]
      (df/down)
      [:a :b]
      (df/down)
      [:b :a]
      (df/down)
      [:a :b])
    (test-traverse
      (df/prune-seen search)
      [nil :a]
      (df/down)
      [:a :b]
      (df/down)
      nil)))
(deftest prune-seen-self
  (let [search (df/dfs {:a [:a]} :a)]
    (test-traverse
      search
      [nil :a]
      (df/down)
      [:a :a]
      (df/down)
      [:a :a])
    (test-traverse
      (df/prune-seen search)
      [nil :a]
      (df/down)
      nil)))
(deftest prune-seen-sister
  (let [search (df/dfs {:a [:b :c :b] :b []} :a)]
    (test-traverse
      search
      [nil :a]
      (df/down)
      [:a :b]
      (df/across)
      [:a :c]
      (df/across)
      [:a :b]
      (df/across)
      nil)
    (test-traverse
      (df/prune-seen search)
      [nil :a]
      (df/down)
      [:a :b]
      (df/across)
      [:a :c]
      (df/across)
      nil)))
(deftest prune-seen-niece
  (let [search (df/dfs {:a [:b :c] :b [:c]} :a)]
    (test-traverse
      search
      [nil :a]
      (df/down)
      [:a :b]
      (df/down)
      [:b :c]
      (df/up)
      [:a :b]
      (df/across)
      [:a :c])
    (test-traverse
      (df/prune-seen search)
      [nil :a]
      (df/down)
      [:a :b]
      (df/down)
      [:b :c]
      (df/up)
      [:a :b]
      (df/across)
      nil)))
(deftest prune-seen-aunt
  (let [search (df/dfs {:a [:b :c] :b [] :c [:b]} :a)]
    (test-traverse
      search
      [nil :a]
      (df/down)
      [:a :b]
      (df/across)
      [:a :c]
      (df/down)
      [:c :b])
    (test-traverse
      (df/prune-seen search)
      [nil :a]
      (df/down)
      [:a :b]
      (df/across)
      [:a :c]
      (df/down)
      nil)))

(deftest step
  (test-traverse
    (df/dfs {:a [:b :c] :b [:x :y]} :a)
    [nil :a]
    (df/step)
    [:a :b]
    (df/step)
    [:b :x]
    (df/step)
    [:b :y]
    (df/step)
    [:a :c]
    (df/step)
    nil))

(deftest preorder
  (is (= (df/preorder (df/dfs {:a [:b :c] :b [:x :y]} :a))
         [:a :b :x :y :c])))
(deftest preorder-pruned
  (is (= (df/preorder (df/dfs {:a [:b :c] :b [:x :a :y] :c [:x]} :a))
         [:a :b :x :y :c])))
(deftest preorder-tree
  (is (= (df/preorder-tree (df/dfs {:a [:b :c] :b [:x :y] :c [:x]} :a))
         [:a :b :x :y :c :x])))
