(ns gmwbot.analyze
  (:use clojure.set)
  (:require [gmwbot.df :as df]))

;; LL(1) parsing

(def ^:private recursion-poison (gensym 'recursion_poison_))
(defn recurser
  "Returns a function for traversing a labelled directed acyclic graph,
  computing a value for each node based on its label and the values
  of its children.  The returned function f is invoked as (f memo
  node), where node is a node in the graph and memo is a map node ->
  value, and this call returns memo with entries added (at least
  an entry for node, and possibly more).  The children of a node
  are determined by calling (children node).  The label of a node is
  determined by calling (label node).  The value of a node is computed
  by logic somewhat like that of reduce: first, the initial value is
  (combine (label node)); then, for each child of node, the value
  of the child is computed recursively, and the value of the current
  node is updated to (combine (label node) prev-value child-value).
  The function combine may short-circuit by returning (reduced x);
  then the values of any further children of the current node will not
  be computed (unless needed elsewhere).  The returned function throws
  IllegalArgumentException if it encounters a cycle in the graph."
  [label children combine]
  (fn f [memo node]
    (let [val (memo node)]
      (cond (= val recursion-poison)
              (throw (IllegalArgumentException. (str "recursion " node)))
            (not (nil? val))
              memo
            true
              (let [lab (label node)]
                (loop [memo (assoc memo node recursion-poison)
                       chile (seq (children node))
                       val (combine lab)]
                  (if (nil? chile)
                    (assoc memo node val)
                    (let [child (first chile)
                          new-memo (f memo child)
                          new-val (combine lab val (new-memo child))]
                      (if (reduced? new-val)
                        (assoc new-memo node @new-val)
                        (recur new-memo (next chile) new-val))))))))))
(defn combiner [f]
  (fn ([label] label)
      ([label x y] (f x y))))
(defn all
  ([] true)
  ([x y] (if (and x y) true (reduced false))))
(defn any
  ([] false)
  ([x y] (if (or x y) (reduced true) false)))

(defn make-nullable?
  "Returns a function which takes one or more symbols as arguments
  and returns whether that sequence of symbols is nullable in the
  given grammar.  The returned function is memoized."
  [productions]
  ;; We use df/memo-reduce with a virtual graph where the symbols
  ;; (terminal and nonterminal) of the grammar appear as nodes
  ;; [:single symbol] and the right-hand sides of productions
  ;; appear as nodes [:list symbols].  The children of a symbol
  ;; node are the nodes representing its right-hand sides; the
  ;; children of a right-hand-side node are the nodes representing
  ;; the symbols in the expansion.  Nullability is then a matter of
  ;; evaluation: a symbol is nullable if any of its children are,
  ;; and a right-hand side is nullable if all of its children are.
  ;; (Nonterminal symbols have no children, so they are not nullable;
  ;; empty right-hand sides have no children, so they *are* nullable.)
  (let [memo (atom {})
        ensure (fn [memo target]
                 (df/memo-reduce memo
                                 (fn [state x]
                                   (if (= state x)
                                     state
                                     (reduced x)))
                                 (fn [[tag _]] (= :list tag))
                                 (df/dfc (fn [[tag x]]
                                           (case tag
                                             :single (map #(list :list %) (productions x))
                                             :list (map #(list :single %) x)
                                             (assert false)))
                                         target)))]
    (fn [& symbols]
      (let [k [:list symbols]]
        (get (swap! memo ensure k) k)))))

(defn nullables
  "The nullable nonterminal symbols of the given grammar, which is
  a map nonterminal -> list of expansions, where an expansion is a
  (possibly empty) list of nonterminals and terminals."
  [productions]
  (let [nullable? (make-nullable? productions)]
    (filter nullable? (keys productions))))

(defn- take-until [pred xs]
  (lazy-seq
    (when (seq xs)
      (let [x (first xs)]
        (if (pred x)
          [x]
          (cons x (take-until pred (rest xs))))))))
(defn make-first-set
  [productions nullable?]
  (let [memo (atom {})
        ensure (fn [memo target]
                 (df/memo-reduce memo
                                 union
                                 (fn [[tag x]]
                                   (if (and (= tag :single)
                                            (nil? (productions x)))
                                     #{x}
                                     #{}))
                                 (df/dfc
                                   (fn [[tag x]]
                                     (if (= tag :single)
                                       (map #(list :list %) (productions x))
                                       (map #(list :single %) (take-until #(not (nullable? %)) x))))
                                   target)))]
    (fn [& symbols]
      (let [k [:list symbols]]
        (get (swap! memo ensure k) k)))))
(defn first-sets
  ([productions]
    (first-sets productions (make-nullable? productions)))
  ([productions nullable?]
    (let [first-set (make-first-set productions nullable?)]
      (->> (keys productions)
           (map (juxt identity first-set))
           (into {})))))
