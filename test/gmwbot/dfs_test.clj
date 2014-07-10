(ns gmwbot.dfs-test
  (:use clojure.test)
  (:require [gmwbot.dfs :as df]))

(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([search-sym f node]
    `(do
       (if-let [node# ~node]
         (is (= node# (~f ~search-sym)))
         (is (nil? ~search-sym)))))
  ([search-sym f node form]
    (concat (test-traverse-clause search-sym f node)
            `((~(first form) ~search-sym ~@(rest form))))))
(defmacro test-traverse [dfs f & forms]
  "Test a sequence of maneuvers in a depth-first search.  dfs is an
  implementation of gmwbot.dfs/DepthFirstSearch; f is a callable taking
  a DepthFirstSearch as an argument.  The remaining forms describe a
  sequence of tests, alternating between the expected value of (f dfs)
  and a form into which dfs will be threaded as the first argument;
  the value of the resulting form will be used as the dfs for the
  subsequent forms.  For example,
    (require '[gmwbot.dfs :as df])
    (test-traverse
      (df/dfs {:a [:b]} :a)
      df/current
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
           ~@(map #(apply test-traverse-clause search-sym f %)
                  (partition-all 2 forms)))))

(deftest down
  (test-traverse
    (df/dfs {:a [:b] :b [:c]} :a)
    df/current
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
    df/current
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
    df/current
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
    df/current
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
    df/current
    :a
    (df/scan-children #(= :c %))
    :c)
  (test-traverse
    (df/dfs {:a [:b :c :d]} :a)
    df/current
    :a
    (df/scan-children #(= :z %))
    nil))

(deftest prune-seen-parent
  (let [search (df/dfs {:a [:b] :b [:a]} :a)]
    (test-traverse
      search
      df/current
      :a
      (df/down)
      :b
      (df/down)
      :a
      (df/down)
      :b)
    (test-traverse
      (df/prune-seen search)
      df/current
      :a
      (df/down)
      :b
      (df/down)
      nil)))
(deftest prune-seen-self
  (let [search (df/dfs {:a [:a]} :a)]
    (test-traverse
      search
      df/current
      :a
      (df/down)
      :a
      (df/down)
      :a)
    (test-traverse
      (df/prune-seen search)
      df/current
      :a
      (df/down)
      nil)))
(deftest prune-seen-sister
  (let [search (df/dfs {:a [:b :c :b] :b []} :a)]
    (test-traverse
      search
      df/current
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
      df/current
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
      df/current
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
      df/current
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
      df/current
      :a
      (df/down)
      :b
      (df/across)
      :c
      (df/down)
      :b)
    (test-traverse
      (df/prune-seen search)
      df/current
      :a
      (df/down)
      :b
      (df/across)
      :c
      (df/down)
      nil)))

(deftest fail-on-loop-down
  (let [verge (-> (df/dfs  {:a [:a]} :a)
                  (df/fail-on-loop))]
    (is (thrown? IllegalStateException (df/down verge)))))
(deftest fail-on-loop-across
  (let [verge (-> (df/dfs  {:a [:b :b :a]} :a)
                  (df/fail-on-loop)
                  (df/down)
                  (df/across))]
    (is (thrown? IllegalStateException (df/across verge)))))
(deftest fail-on-loop-no-loop
  (is (= [:a :b :c :b]
         (df/preorder-tree
           (df/fail-on-loop
             (df/dfs {:a [:b :c] :c [:b]} :a))))))

(deftest stepping
  (as-> (df/stepper (df/dfs {:a [:b :c] :c [:b]} :a))
        stepper
        (do
          (is (= :a (df/current stepper)))
          (is (= true (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :b (df/current stepper)))
          (is (= true (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :b (df/current stepper)))
          (is (= false (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :c (df/current stepper)))
          (is (= true (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :b (df/current stepper)))
          (is (= true (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :b (df/current stepper)))
          (is (= false (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :c (df/current stepper)))
          (is (= false (df/inbound? stepper)))
          (df/step stepper))
        (do
          (is (= :a (df/current stepper)))
          (is (= false (df/inbound? stepper)))
          (df/step stepper))
        (is nil? stepper)))

(deftest preorder
  (is (= (df/preorder (df/dfs {:a [:b :c] :b [:x :y]} :a))
         [:a :b :x :y :c])))
(deftest preorder-pruned
  (is (= (df/preorder (df/dfs {:a [:b :c] :b [:x :a :y] :c [:x]} :a))
         [:a :b :x :y :c])))
(deftest preorder-tree
  (is (= (df/preorder-tree (df/dfs {:a [:b :c] :b [:x :y] :c [:x]} :a))
         [:a :b :x :y :c :x])))

(deftest postorder
  (is (= (df/postorder (df/dfs {:a [:b :c] :b [:x :y]} :a))
         [:x :y :b :c :a])))
(deftest postorder-pruned
  (is (= (df/postorder (df/dfs {:a [:b :c] :b [:x :a :y] :c [:x]} :a))
         [:x :y :b :c :a])))
(deftest postorder-tree
  (is (= (df/postorder-tree (df/dfs {:a [:b :c] :b [:x :y] :c [:x]} :a))
         [:x :y :b :x :c :a])))
