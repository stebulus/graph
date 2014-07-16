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

(deftest prune-children
  (let [cursor (df/down (df/dfc {1 [2 3] 2 [4 5]} 1))
        pruned (df/prune-children cursor)]
    (is (= 4 (df/current (df/down cursor))))
    (is (nil? (df/down pruned)))
    (is (= 3 (df/current (df/across cursor))))
    (is (= 3 (df/current (df/across pruned))))
    (is (= 1 (df/current (df/up cursor))))
    (is (= 1 (df/current (df/up pruned))))))
(deftest prune-siblings
  (let [cursor (df/down (df/dfc {1 [2 3] 2 [4 5]} 1))
        pruned (df/prune-siblings cursor)]
    (is (= 1 (df/current (df/up cursor))))
    (is (= 1 (df/current (df/up pruned))))
    (is (= 3 (df/current (df/across cursor))))
    (is (nil? (df/across pruned)))
    (test-traverse cursor df/current
      2 (df/down) 4 (df/across) 5 (df/up) 2 (df/across) 3)
    (test-traverse pruned df/current
      2 (df/down) 4 (df/across) 5 (df/up) 2 (df/across) nil)))

(deftest dfreductions
  (test-traverse
    (df/reductions #(conj %1 [:in %2])
                   #(conj %1 [:out %2])
                   []
                   (df/dfc {1 [2 3] 2 [4 5]} 1))
    df/current
    [[:in 1]]
    (df/step)
    [[:in 1] [:in 2]]
    (df/step)
    [[:in 1] [:in 2] [:in 4]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5] [:out 2]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5] [:out 2] [:in 3]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5] [:out 2] [:in 3] [:out 3]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5] [:out 2] [:in 3] [:out 3] [:out 1]]
    (df/step)
    nil))
(deftest dfreductions-shortcircuit-in
  (test-traverse
    (df/reductions #((if (= 2 %2) reduced identity) (conj %1 [:in %2]))
                   #(conj %1 [:out %2])
                   []
                   (df/dfc {1 [2 3] 2 [4 5]} 1))
    df/current
    [[:in 1]]
    (df/step)
    [[:in 1] [:in 2]]
    (df/step)
    [[:in 1] [:in 2] [:out 2]]
    (df/step)
    [[:in 1] [:in 2] [:out 2] [:in 3]]
    (df/step)
    [[:in 1] [:in 2] [:out 2] [:in 3] [:out 3]]
    (df/step)
    [[:in 1] [:in 2] [:out 2] [:in 3] [:out 3] [:out 1]]
    (df/step)
    nil))
(deftest dfreductions-shortcircuit-out
  (test-traverse
    (df/reductions #(conj %1 [:in %2])
                   #((if (= 2 %2) reduced identity) (conj %1 [:out %2]))
                   []
                   (df/dfc {1 [2 3] 2 [4 5]} 1))
    df/current
    [[:in 1]]
    (df/step)
    [[:in 1] [:in 2]]
    (df/step)
    [[:in 1] [:in 2] [:in 4]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5] [:out 2]]
    (df/step)
    [[:in 1] [:in 2] [:in 4] [:out 4] [:in 5] [:out 5] [:out 2] [:out 1]]
    (df/step)
    nil))
(deftest dfreductions-shortcircuit-immed
  (test-traverse
    (df/reductions #(reduced (conj %1 [:in %2]))
                   #(conj %1 [:out %2])
                   []
                   (df/dfc {1 [2 3] 2 [4 5]} 1))
    df/current
    [[:in 1]]
    (df/step)
    [[:in 1] [:out 1]]
    (df/step)
    nil))

(deftest dfreduce
  (is (= [:a [:b [:x] [:y]] [:c]]
         (df/reduce conj
                    (fn [x] [x])
                    (df/dfc {:a [:b :c] :b [:x :y]} :a)))))
(deftest dfreduce-shortcircuit-children
  (let [nox (->> (df/dfc {:a [:b :c] :b [:x :y] :c [:q]} :a)
                 (df/never #(= :x (df/current %))))]
    (is (thrown? AssertionError (df/down (df/down nox))))
    (is (thrown? AssertionError
                 (df/reduce conj
                            (fn [x] [x])
                            nox)))
    (is (= [:a [:b] [:c [:q]]]
           (df/reduce conj
                      (fn [x]
                          (if (= :b x)
                            (reduced [x])
                            [x]))
                      nox)))))
(deftest dfreduce-shortcircuit-siblings
  (let [nox (->> (df/dfc {:a [:b :c :x] :b [:p] :x [:q :r]} :a)
                 (df/never #(= :x (df/current %))))]
    (is (thrown? AssertionError
                 (->> (df/down nox)
                      (df/scan df/across (constantly false)))))
    (is (thrown? AssertionError
                 (df/reduce conj
                            (fn [x] [x])
                            nox)))
    (is (= [:a [:b [:p]] [:c]]
           (df/reduce (fn [L x]
                          (let [newL (conj L x)]
                            (if (= :c (first x))
                              (reduced newL)
                              newL)))
                      (fn [x] [x])
                      nox)))))
(deftest dfreduce-loop
  (is (thrown? AssertionError
               (df/reduce +
                          identity
                          (df/dfc {1 [2 3] 2 [4 5] 3 [4 1]} 1))))
  (is (= (+ 1 2 3 4 5)
         (df/reduce +
                    (fn [curr]
                        (if (= 3 curr)
                          (reduced curr)
                          curr))
                    (df/dfc {1 [2 3] 2 [4 5] 3 [4 1]} 1))))
  (is (= (+ 1 2 3 4 5 7)
         (df/reduce (fn [x y]
                        (let [val (+ x y)]
                          (if (= 7 y)
                            (reduced val)
                            val)))
                    identity
                    (df/dfc {1 [2 3] 2 [4 5] 3 [7 1]} 1)))))
(deftest super-reduce
  ; A technique for short-circuiting the entire rest of the tree.
  (let [cursor (->> (df/dfc {2 [4 3] 4 [6 8 5 7] 8 [10 12 1] 12 [9]} 2)
                    (df/never #(odd? (df/current %)) "odd"))]
    (is (thrown? AssertionError (df/reduce +
                                           identity
                                           cursor)))
    (is (= (+ 2 4 6 8 10 12)
           @(df/reduce (fn [x y]
                           (if (reduced? y)
                             (reduced (reduced (+ x @y)))
                             (+ x y)))
                       (fn [curr]
                           (if (= curr 12)
                             (reduced (reduced curr))
                             curr))
                       cursor)))))

(deftest memo-reduce
  (is (= {1 15, 2 11, 3 3, 4 4, 5 5}
         (df/memo-reduce +
                         identity
                         (df/dfc {1 [2 3] 2 [4 5]} 1))))
  (is (= {1 5, 2 -2, 3 6}
         (df/memo-reduce {2 -2, 3 6}
                         +
                         identity
                         (df/dfc {1 [2 3] 2 [4 5]} 1)))))
(deftest memo-reduce-shortcircuit-children
  (let [nox (->> (df/dfc {:a [:b :c :d] :b [:x :y] :c [:q] :d [:x]} :a)
                 (df/never #(= :x (df/current %))))]
    (is (thrown? AssertionError (df/down (df/down nox))))
    (is (thrown? AssertionError
                 (df/memo-reduce conj
                                 (fn [x] [x])
                                 nox)))
    (is (= {:a [:a [:b] [:c [:q]] [:no]]
            :b [:b]
            :c [:c [:q]]
            :d [:no]
            :q [:q]}
           (df/memo-reduce {:d [:no]}
                           conj
                           (fn [x]
                               (if (= :b x)
                                 (reduced [x])
                                 [x]))
                           nox)))))
(deftest memo-reduce-shortcircuit-siblings
  (let [nox (->> (df/dfc {:a [:b :c :x] :b [:p] :c [:m :n] :x [:q :r]} :a)
                 (df/never #(= :x (df/current %))))]
    (is (thrown? AssertionError
                 (->> (df/down nox)
                      (df/scan df/across (constantly false)))))
    (is (thrown? AssertionError
                 (df/memo-reduce conj
                                 (fn [x] [x])
                                 nox)))
    (is (= {:a [:a [:no] [:c [:m] [:n]]]
            :b [:no]
            :c [:c [:m] [:n]]
            :m [:m]
            :n [:n]}
           (df/memo-reduce {:b [:no]}
                           (fn [L x]
                             (let [newL (conj L x)]
                               (if (= :c (first x))
                                 (reduced newL)
                                 newL)))
                           (fn [x] [x])
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

(deftest as-siblings
  (let [graph {1 [3 5] 2 [4 6]}
        cursor (df/as-siblings [(df/dfc graph 1) (df/dfc graph 2)])]
    (test-traverse cursor df/current
      1 (df/across) 2 (df/across) nil)
    (test-traverse cursor df/current
      1 (df/up) nil)
    (test-traverse cursor df/current
      1 (df/across) 2 (df/up) nil)
    (test-traverse cursor df/current
      1 (df/down) 3 (df/up) 1 (df/across) 2 (df/down) 4 (df/up) 2 (df/across) nil)
    (is (= [1 3 5 2 4 6] (df/preorder-tree cursor)))
    (is (= [1 3 5] (df/preorder-tree (df/reroot cursor))))
    (is (= [2 4 6] (df/preorder-tree (df/reroot (df/across cursor)))))))
(deftest as-siblings-with-siblings
  (let [graph {1 [3 5] 3 [7 9] 2 [4 6]}
        cursor (df/as-siblings [(df/down (df/dfc graph 1))
                                (df/dfc graph 2)])]
    (test-traverse cursor df/current
      3 (df/across) 5 (df/across) 2 (df/across) nil)
    (is (= [3 7 9 5 2 4 6] (df/preorder-tree cursor)))))
