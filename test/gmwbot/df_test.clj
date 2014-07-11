(ns gmwbot.df-test
  (:use clojure.test)
  (:require [gmwbot.df :as df]))

(defn- test-traverse-clause
  "Helper function for macro test-traverse."
  ([cursor-sym f node]
    `(do
       (if-let [node# ~node]
         (do
           (is (some? ~cursor-sym))
           (is (= node# (~f ~cursor-sym))))
         (is (nil? ~cursor-sym)))))
  ([cursor-sym f node form]
    (concat (test-traverse-clause cursor-sym f node)
            (list (concat form (list cursor-sym))))))
(defmacro test-traverse [dfc f & forms]
  "Test a sequence of maneuvers in a depth-first traversal.  dfc is an
  implementation of gmwbot.df/DepthFirstCursor; f is a callable taking
  a DepthFirstCursor as an argument.  The remaining forms describe a
  sequence of tests, alternating between the expected value of (f dfc)
  and a form into which dfc will be threaded as the last argument;
  the value of the resulting form will be used as the dfc for the
  subsequent forms.  For example,
    (require '[gmwbot.df :as df])
    (test-traverse
      (df/dfc {:a [:b]} :a)
      df/current
      :a
      (df/down)
      :b
      (df/down)
      nil)
  verifies the initial state, moves down, verifies the resulting state,
  tries to move down again, and verifies that the second move failed."
  (let [cursor-sym (gensym 'cursor_)]
    `(as-> ~dfc
           ~cursor-sym
           ~@(map #(apply test-traverse-clause cursor-sym f %)
                  (partition-all 2 forms)))))

(deftest down
  (test-traverse
    (df/dfc {:a [:b] :b [:c]} :a)
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
    (df/dfc {:a [:b :c :d]} :a)
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
    (df/dfc {:a [:b] :b [:c]} :a)
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

(deftest reroot-up
  (test-traverse
    (df/dfc {:a [:b :c] :b [:x :y]} :a)
    df/current
    :a
    (df/down)
    :b
    (df/reroot)
    :b
    (df/up)
    nil))
(deftest reroot-across
  (test-traverse
    (df/dfc {:a [:b :c] :b [:x :y]} :a)
    df/current
    :a
    (df/down)
    :b
    (df/reroot)
    :b
    (df/across)
    nil))
(deftest reroot-preorder
  (is (= [:b :x :y]
         (->> (df/dfc {:a [:b :c] :b [:x :y]} :a)
              (df/down)
              (df/reroot)
              (df/preorder-tree)))))

(deftest scan-across
  (test-traverse
    (df/dfc {:a [:b :c :d :e]} :a)
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
    (df/dfc {:a [:b :c :d :e]} :a)
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
    (df/dfc {:a [:b :c] :b [:x :y] :c [:a]} :a)
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

(deftest dfreduce
  (is (= [:a [:b [:x] [:y]] [:c]]
         (df/reduce conj
                    (fn [x] [(df/current x)])
                    (df/dfc {:a [:b :c] :b [:x :y]} :a)))))
(deftest dfreduce-shortcircuit-children
  (let [nox (->> (df/dfc {:a [:b :c] :b [:x :y] :c [:q]} :a)
                 (df/never #(= :x (df/current %))))]
    (is (thrown? AssertionError (df/down (df/down nox))))
    (is (thrown? AssertionError
                 (df/reduce conj
                            (fn [x] [(df/current x)])
                            nox)))
    (is (= [:a [:b] [:c [:q]]]
           (df/reduce conj
                      (fn [x]
                          (let [curr (df/current x)]
                            (if (= :b curr)
                              (reduced [curr])
                              [curr])))
                      nox)))))
(deftest dfreduce-shortcircuit-siblings
  (let [nox (->> (df/dfc {:a [:b :c :x] :b [:p] :x [:q :r]} :a)
                 (df/never #(= :x (df/current %))))]
    (is (thrown? AssertionError
                 (->> (df/down nox)
                      (df/scan df/across (constantly false)))))
    (is (thrown? AssertionError
                 (df/reduce conj
                            (fn [x] [(df/current x)])
                            nox)))
    (is (= [:a [:b [:p]] [:c]]
           (df/reduce (fn [L x]
                          (let [newL (conj L x)]
                            (if (= :c (first x))
                              (reduced newL)
                              newL)))
                      (fn [x] [(df/current x)])
                      nox)))))

(deftest doeach
  (let [x (atom [])]
    (test-traverse
      (df/doeach
        #(swap! x conj (df/current %))
        (df/dfc {:a [:b :c] :b [:x :y]} :a))
      (fn [s] @x)
      [:a]
      (df/down)
      [:a :b]
      (df/across)
      [:a :b :c]
      (df/up)
      [:a :b :c :a])))

(deftest never-down
  (let [verge (->> (df/dfc {:a [:b :c] :b [:x :y]} :a)
                   (df/never #(= :b (df/current %))))]
    (is (thrown? AssertionError (df/down verge)))))
(deftest never-across
  (let [verge (->> (df/dfc {:a [:b :c] :b [:x :y]} :a)
                   (df/never #(= :c (df/current %)))
                   (df/down))]
    (is (thrown? AssertionError (df/across verge)))))
(deftest never-up
  (let [verge (->> (df/dfc {:a [:b :c] :b [:x :y]} :a)
                   (df/down)
                   (df/down)
                   (df/never #(= :b (df/current %)))
                   (df/across))]
    (is (thrown? AssertionError (df/up verge)))))
(deftest never-immediate
  (is (thrown? AssertionError
               (df/never #(= :a (df/current %))
                         (df/dfc {:a [:b]} :a)))))
(deftest never-reroot
  (let [atb (->> (df/dfc {:a [:b :c] :b [:x :y] :c [:bad]} :a)
                 (df/never #(= :bad (df/current %)))
                 (df/down))]
    (is (thrown? AssertionError (dorun (df/preorder-tree atb))))
    (is (= [:b :x :y] (df/preorder-tree (df/reroot atb))))))
(deftest never-reroot-immediate
  (let [verge (->> (df/dfc {:a [:b :c]} :a)
                   (df/down)
                   (df/never #(nil? (df/up %))))]
    (is (thrown? AssertionError (df/reroot verge)))))

(deftest seen-parent
  (test-traverse
    (df/record-seen (df/dfc {:a [:b] :b [:a]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/down)
    [:a true]))
(deftest seen-self
  (test-traverse
    (df/record-seen (df/dfc {:a [:a]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:a true]))
(deftest seen-sibling
  (test-traverse
    (df/record-seen (df/dfc {:a [:b :b]} :a))
    (juxt df/current df/seen?)
    [:a false]
    (df/down)
    [:b false]
    (df/across)
    [:b true]))
(deftest seen-niece
  (test-traverse
    (df/record-seen (df/dfc {:a [:b :c] :b [:c]} :a))
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
    (df/record-seen (df/dfc {:a [:b :c] :c [:b]} :a))
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
    (df/record-seen (df/dfc {:a [:b :c] :b [:x] :c [:x]} :a))
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
(deftest seen-reroot
  (let [atc (->> (df/dfc {:a [:b :c] :b [:x :y] :c [:x]} :a)
                 (df/record-seen)
                 (df/stepper)
                 (df/scan df/step #(= :c (df/current %)))
                 (df/unwrap))]
    (is (df/seen? (df/down atc)))
    (is (not (df/seen? (df/down (df/reroot atc)))))))

(deftest prune-down
  (test-traverse
    (df/prune #(= :b (df/current %))
              (df/dfc {:a [:b :c] :b [:x :y]} :a))
    df/current
    :a
    (df/down)
    :c)
  (test-traverse
    (df/prune #(= :b (df/current %))
              (df/dfc {:a [:b :b] :b [:x :y]} :a))
    df/current
    :a
    (df/down)
    nil))
(deftest prune-across
  (test-traverse
    (df/prune #(= :c (df/current %))
              (df/dfc {:a [:b :c :d] :c [:x :y]} :a))
    df/current
    :a
    (df/down)
    :b
    (df/across)
    :d)
  (test-traverse
    (df/prune #(= :c (df/current %))
              (df/dfc {:a [:b :c] :c [:x :y]} :a))
    df/current
    :a
    (df/down)
    :b
    (df/across)
    nil))
(deftest prune-up
  (test-traverse
    (df/prune #(= :c (df/current %))
              (-> (df/dfc {:a [:b :c :d] :c [:x :c :y]} :a)
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

(deftest stepping
  (test-traverse
    (df/stepper (df/dfc {:a [:b :c] :c [:b]} :a))
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
  (is (= (df/preorder (df/dfc {:a [:b :c] :b [:x :y]} :a))
         [:a :b :x :y :c])))
(deftest preorder-pruned
  (is (= (df/preorder (df/dfc {:a [:b :c] :b [:x :a :y] :c [:x]} :a))
         [:a :b :x :y :c])))
(deftest preorder-tree
  (is (= (df/preorder-tree (df/dfc {:a [:b :c] :b [:x :y] :c [:x]} :a))
         [:a :b :x :y :c :x])))

(deftest postorder
  (is (= (df/postorder (df/dfc {:a [:b :c] :b [:x :y]} :a))
         [:x :y :b :c :a])))
(deftest postorder-pruned
  (is (= (df/postorder (df/dfc {:a [:b :c] :b [:x :a :y] :c [:x]} :a))
         [:x :y :b :c :a])))
(deftest postorder-tree
  (is (= (df/postorder-tree (df/dfc {:a [:b :c] :b [:x :y] :c [:x]} :a))
         [:x :y :b :x :c :a])))
