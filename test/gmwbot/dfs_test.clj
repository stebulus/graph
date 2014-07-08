(ns gmwbot.dfs-test
  (:use clojure.test
        gmwbot.dfs))
(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([search-sym failed edge]
    `(do
       (is (= ~failed (failed? ~search-sym)))
       (is (= ~edge (last-edge ~search-sym)))))
  ([search-sym failed edge func]
    (concat (test-traverse-clause search-sym failed edge)
            (list (list func search-sym)))))
(defmacro test-traverse [dfs & forms]
  "Test a sequence of maneuvers in a depth-first search.  The first
  argument is an expression evaluating to an implementation
  of gmwbot.dfs/DepthFirstSearch.  The remaining forms describe a
  sequence of tests, in threes: in each triple of forms, the first
  form is the value that should be returned by (failed? dfs), the
  second is the value that should be returned by (last-edge dfs),
  and the third is a function f; the value of (f dfs) will be used
  as the dfs for the next forms.  For example,
    (test-traverse
      (dfs {:a []} :a)
      false [nil :a]
      down
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
(deftest dfs-down
  (test-traverse
    (dfs {:a [:b] :b [:c]} :a)
    false [nil :a]
    down
    false [:a :b]
    down
    false [:b :c]
    down
    true [:b :c]))
(deftest dfs-across
  (test-traverse
    (dfs {:a [:b :c :d]} :a)
    false [nil :a]
    down
    false [:a :b]
    across
    false [:a :c]
    across
    false [:a :d]
    across
    true [:a :d]))
(deftest dfs-up
  (test-traverse
    (dfs {:a [:b] :b [:c]} :a)
    false [nil :a]
    down
    false [:a :b]
    down
    false [:b :c]
    up
    false [:a :b]
    up
    false [nil :a]
    up
    true [nil :a]))
