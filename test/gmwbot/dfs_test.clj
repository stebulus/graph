(ns gmwbot.dfs-test
  (:use clojure.test)
  (:require [gmwbot.dfs :as df]))

(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([search-sym node]
    `(do
       (if-let [node# ~node]
         (is (= node# (df/current ~search-sym)))
         (is (nil? ~search-sym)))))
  ([search-sym node form]
    (concat (test-traverse-clause search-sym node)
            `((~(first form) ~search-sym ~@(rest form))))))
(defmacro test-traverse [dfs & forms]
  "Test a sequence of maneuvers in a depth-first search.  The first
  argument is an expression evaluating to an implementation of
  gmwbot.dfs/DepthFirstSearch.  The remaining forms describe a sequence
  of tests, alternating between the node expected from (current dfs)
  and a form into which dfs will be threaded as the first argument;
  the value of the resulting form will be used as the dfs for the
  subsequent forms.  For example,
    (require '[gmwbot.dfs :as df])
    (test-traverse
      (df/dfs {:a [:b]} :a)
      :a
      (df/down)
      :b
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
    :a
    (df/down)
    :b
    (df/down)
    :c
    (df/down)
    nil))
(deftest across
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    :a
    (df/down)
    :b
    (df/across)
    :c
    (df/across)
    :d
    (df/across)
    nil))
(deftest up
  (test-traverse
    (df/dfs {:a [:b] :b [:c]} :a)
    :a
    (df/down)
    :b
    (df/down)
    :c
    (df/up)
    :b
    (df/up)
    :a
    (df/up)
    nil))

(deftest scan-across
  (test-traverse
    (df/dfs {:a [:b :c :d :e]} :a)
    :a
    (df/down)
    :b
    (df/scan-across #(= :b %))
    :b
    (df/scan-across #(= :d %))
    :d
    (df/scan-across #(= :f %))
    nil))
(deftest scan-children
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    :a
    (df/scan-children #(= :c %))
    :c)
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    :a
    (df/scan-children #(= :z %))
    nil))

(deftest prune-seen-parent
  (let [search (df/dfs {:a [:b] :b [:a]} :a)]
    (test-traverse
      search
      :a
      (df/down)
      :b
      (df/down)
      :a
      (df/down)
      :b)
    (test-traverse
      (df/prune-seen search)
      :a
      (df/down)
      :b
      (df/down)
      nil)))
(deftest prune-seen-self
  (let [search (df/dfs {:a [:a]} :a)]
    (test-traverse
      search
      :a
      (df/down)
      :a
      (df/down)
      :a)
    (test-traverse
      (df/prune-seen search)
      :a
      (df/down)
      nil)))
(deftest prune-seen-sister
  (let [search (df/dfs {:a [:b :c :b] :b []} :a)]
    (test-traverse
      search
      :a
      (df/down)
      :b
      (df/across)
      :c
      (df/across)
      :b
      (df/across)
      nil)
    (test-traverse
      (df/prune-seen search)
      :a
      (df/down)
      :b
      (df/across)
      :c
      (df/across)
      nil)))
(deftest prune-seen-niece
  (let [search (df/dfs {:a [:b :c] :b [:c]} :a)]
    (test-traverse
      search
      :a
      (df/down)
      :b
      (df/down)
      :c
      (df/up)
      :b
      (df/across)
      :c)
    (test-traverse
      (df/prune-seen search)
      :a
      (df/down)
      :b
      (df/down)
      :c
      (df/up)
      :b
      (df/across)
      nil)))
(deftest prune-seen-aunt
  (let [search (df/dfs {:a [:b :c] :b [] :c [:b]} :a)]
    (test-traverse
      search
      :a
      (df/down)
      :b
      (df/across)
      :c
      (df/down)
      :b)
    (test-traverse
      (df/prune-seen search)
      :a
      (df/down)
      :b
      (df/across)
      :c
      (df/down)
      nil)))

(deftest step
  (test-traverse
    (df/dfs {:a [:b :c] :b [:x :y]} :a)
    :a
    (df/step)
    :b
    (df/step)
    :x
    (df/step)
    :y
    (df/step)
    :c
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
