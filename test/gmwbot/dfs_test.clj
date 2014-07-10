(ns gmwbot.dfs-test
  (:use clojure.test)
  (:require [gmwbot.dfs :as df]))

(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([search-sym f node]
    `(do
       (if-let [node# ~node]
         (do
           (is (some? ~search-sym))
           (is (= node# (~f ~search-sym))))
         (is (nil? ~search-sym)))))
  ([search-sym f node form]
    (concat (test-traverse-clause search-sym f node)
            (list (concat form (list search-sym))))))
(defmacro test-traverse [dfs f & forms]
  "Test a sequence of maneuvers in a depth-first search.  dfs is an
  implementation of gmwbot.dfs/DepthFirstSearch; f is a callable taking
  a DepthFirstSearch as an argument.  The remaining forms describe a
  sequence of tests, alternating between the expected value of (f dfs)
  and a form into which dfs will be threaded as the last argument;
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
    (df/scan df/across #(= :b (df/current %)))
    :b
    (df/scan df/across #(= :d (df/current %)))
    :d
    (df/scan df/across #(= :f (df/current %)))
    nil))
(deftest skip-across
  (test-traverse
    (df/dfs {:a [:b :c :d :e]} :a)
    df/current
    :a
    (df/down)
    :b
    (df/skip df/across #(= :c (df/current %)))
    :b
    (df/skip df/across #(= :b (df/current %)))
    :c
    (df/skip df/across #(not (= :a (df/current %))))
    nil))

(deftest loop?
  (test-traverse
    (df/dfs {:a [:b :c] :b [:x :y] :c [:a]} :a)
    (juxt df/current df/loop?)
    [:a nil]
    (df/down)
    [:b nil]
    (df/across)
    [:c nil]
    (df/down)
    [:a true]
    (df/down)
    [:b nil]
    (df/across)
    [:c true]))

(deftest never-down
  (let [verge (->> (df/dfs {:a [:b :c] :b [:x :y]} :a)
                   (df/never #(= :b (df/current %))))]
    (is (thrown? AssertionError (df/down verge)))))
(deftest never-across
  (let [verge (->> (df/dfs {:a [:b :c] :b [:x :y]} :a)
                   (df/never #(= :c (df/current %)))
                   (df/down))]
    (is (thrown? AssertionError (df/across verge)))))
(deftest never-up
  (let [verge (->> (df/dfs {:a [:b :c] :b [:x :y]} :a)
                   (df/down)
                   (df/down)
                   (df/never #(= :b (df/current %)))
                   (df/across))]
    (is (thrown? AssertionError (df/up verge)))))
(deftest never-immediate
  (is (thrown? AssertionError
               (df/never #(= :a (df/current %))
                         (df/dfs {:a [:b]} :a)))))

(deftest seen-parent
  (test-traverse
    (df/record-seen (df/dfs {:a [:b] :b [:a]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/down)
    [:a true]))
(deftest seen-self
  (test-traverse
    (df/record-seen (df/dfs {:a [:a]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:a true]))
(deftest seen-sibling
  (test-traverse
    (df/record-seen (df/dfs {:a [:b :b]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/across)
    [:b true]))
(deftest seen-niece
  (test-traverse
    (df/record-seen (df/dfs {:a [:b :c] :b [:c]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/down)
    [:c false]
    (df/up)
    [:b true]
    (df/across)
    [:c true]))
(deftest seen-aunt
  (test-traverse
    (df/record-seen (df/dfs {:a [:b :c] :c [:b]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/across)
    [:c false]
    (df/down)
    [:b true]))
(deftest seen-cousin
  (test-traverse
    (df/record-seen (df/dfs {:a [:b :c] :b [:x] :c [:x]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/down)
    [:x false]
    (df/up)
    [:b true]
    (df/across)
    [:c false]
    (df/down)
    [:x true]))

(deftest prune-down
  (test-traverse
    (df/prune #(= :b (df/current %))
              (df/dfs {:a [:b :c] :b [:x :y]} :a))
    df/current
    :a
    (df/down)
    :c)
  (test-traverse
    (df/prune #(= :b (df/current %))
              (df/dfs {:a [:b :b] :b [:x :y]} :a))
    df/current
    :a
    (df/down)
    nil))
(deftest prune-across
  (test-traverse
    (df/prune #(= :c (df/current %))
              (df/dfs {:a [:b :c :d] :c [:x :y]} :a))
    df/current
    :a
    (df/down)
    :b
    (df/across)
    :d)
  (test-traverse
    (df/prune #(= :c (df/current %))
              (df/dfs {:a [:b :c] :c [:x :y]} :a))
    df/current
    :a
    (df/down)
    :b
    (df/across)
    nil))
(deftest prune-up
  (test-traverse
    (df/prune #(= :c (df/current %))
              (-> (df/dfs {:a [:b :c :d] :c [:x :c :y]} :a)
                  (df/down)
                  (df/across)
                  (df/down)))
    df/current
    :x
    (df/across)
    :y
    (df/up)
    :c
    (df/across)
    :d))

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
  (test-traverse
    (df/stepper (df/dfs {:a [:b :c] :c [:b]} :a))
    (juxt df/current df/inbound?)
    [:a true]
    (df/step)
    [:b true]
    (df/step)
    [:b false]
    (df/step)
    [:c true]
    (df/step)
    [:b true]
    (df/step)
    [:b false]
    (df/step)
    [:c false]
    (df/step)
    [:a false]
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

(deftest postorder
  (is (= (df/postorder (df/dfs {:a [:b :c] :b [:x :y]} :a))
         [:x :y :b :c :a])))
(deftest postorder-pruned
  (is (= (df/postorder (df/dfs {:a [:b :c] :b [:x :a :y] :c [:x]} :a))
         [:x :y :b :c :a])))
(deftest postorder-tree
  (is (= (df/postorder-tree (df/dfs {:a [:b :c] :b [:x :y] :c [:x]} :a))
         [:x :y :b :x :c :a])))
