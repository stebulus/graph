(ns gmwbot.dfs-test
  (:use clojure.test)
  (:require [gmwbot.dfs :as df]))

(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([search-sym success edge]
    `(do
       (is (= ~success (df/success? ~search-sym)))
       (is (= ~edge (df/last-edge ~search-sym)))))
  ([search-sym success edge form]
    (concat (test-traverse-clause search-sym success edge)
            `((~(first form) ~search-sym ~@(rest form))))))
(defmacro test-traverse [dfs & forms]
  "Test a sequence of maneuvers in a depth-first search.  The first
  argument is an expression evaluating to an implementation
  of gmwbot.dfs/DepthFirstSearch.  The remaining forms describe a
  sequence of tests, in threes: in each triple of forms, the first
  form is the value that should be returned by (success? dfs), the
  second is the value that should be returned by (last-edge dfs),
  and the third is a form into which dfs should be threaded as the
  first argument; the value of the resulting form will be used as
  the dfs for the next forms.  For example,
    (require '[gmwbot.dfs :as df])
    (test-traverse
      (df/dfs {:a []} :a)
      false [nil :a]
      (df/down)
      true [nil :a])
  verifies the initial state, tries to move down, and verifies the
  resulting state.  The number of forms should be a multiple of 3,
  except that (as in the example above) the last triple may omit the
  function f, so the number of forms may be of the form 3k+2."
  (let [search-sym (gensym 'search_)]
    `(as-> ~dfs
           ~search-sym
           ~@(map #(apply test-traverse-clause search-sym %)
                  (partition-all 3 forms)))))

(deftest down
  (test-traverse
    (df/dfs {:a [:b] :b [:c]} :a)
    true [nil :a]
    (df/down)
    true [:a :b]
    (df/down)
    true [:b :c]
    (df/down)
    false [:b :c]))
(deftest across
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    true [nil :a]
    (df/down)
    true [:a :b]
    (df/across)
    true [:a :c]
    (df/across)
    true [:a :d]
    (df/across)
    false [:a :d]))
(deftest up
  (test-traverse
    (df/dfs {:a [:b] :b [:c]} :a)
    true [nil :a]
    (df/down)
    true [:a :b]
    (df/down)
    true [:b :c]
    (df/up)
    true [:a :b]
    (df/up)
    true [nil :a]
    (df/up)
    false [nil :a]))

(deftest scan-across
  (test-traverse
    (df/dfs {:a [:b :c :d :e]} :a)
    true [nil :a]
    (df/down)
    true [:a :b]
    (df/scan-across #(= :b %))
    true [:a :b]
    (df/scan-across #(= :d %))
    true [:a :d]
    (df/scan-across #(= :f %))
    false [:a :e]))
(deftest scan-children
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    true [nil :a]
    (df/scan-children #(= :c %))
    true [:a :c])
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    true [nil :a]
    (df/scan-children #(= :z %))
    false [nil :a]))

(deftest prune-seen-parent
  (let [search (df/dfs {:a [:b] :b [:a]} :a)]
    (test-traverse
      search
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/down)
      true [:b :a]
      (df/down)
      true [:a :b])
    (test-traverse
      (df/prune-seen search)
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/down)
      false [:a :b])))
(deftest prune-seen-self
  (let [search (df/dfs {:a [:a]} :a)]
    (test-traverse
      search
      true [nil :a]
      (df/down)
      true [:a :a]
      (df/down)
      true [:a :a])
    (test-traverse
      (df/prune-seen search)
      true [nil :a]
      (df/down)
      false [nil :a])))
(deftest prune-seen-sister
  (let [search (df/dfs {:a [:b :c :b] :b []} :a)]
    (test-traverse
      search
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/across)
      true [:a :c]
      (df/across)
      true [:a :b]
      (df/across)
      false [:a :b])
    (test-traverse
      (df/prune-seen search)
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/across)
      true [:a :c]
      (df/across)
      false [:a :c])))
(deftest prune-seen-niece
  (let [search (df/dfs {:a [:b :c] :b [:c]} :a)]
    (test-traverse
      search
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/down)
      true [:b :c]
      (df/up)
      true [:a :b]
      (df/across)
      true [:a :c])
    (test-traverse
      (df/prune-seen search)
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/down)
      true [:b :c]
      (df/up)
      true [:a :b]
      (df/across)
      false [:a :b])))
(deftest prune-seen-aunt
  (let [search (df/dfs {:a [:b :c] :b [] :c [:b]} :a)]
    (test-traverse
      search
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/across)
      true [:a :c]
      (df/down)
      true [:c :b])
    (test-traverse
      (df/prune-seen search)
      true [nil :a]
      (df/down)
      true [:a :b]
      (df/across)
      true [:a :c]
      (df/down)
      false [:a :c])))
