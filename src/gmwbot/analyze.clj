(ns gmwbot.analyze
  (:use clojure.set)
  (:require [gmwbot.df :as df]))

;; A few graph algorithms

(defn scc [graph]
  ;; Kosaraju's algorithm
  (let [transpose (apply merge-with
                         concat
                         (for [[k vs] graph v vs] {v [k]}))]
    (loop [stack (into []
                       (df/postorder
                         (df/as-siblings
                           (map #(df/dfc graph %)
                                (keys graph)))))
           sccs #{}
           vscc {}]
      (if (empty? stack)
        sccs
        (let [v (peek stack)]
          (if (contains? vscc v)
            (recur (pop stack) sccs vscc)
            (let [newscc (->> (df/dfc transpose v)
                              (df/prune #(vscc (df/current %)))
                              (df/preorder)
                              (into #{}))]
              (recur (pop stack)
                     (conj sccs newscc)
                     (into vscc (for [node newscc] [node newscc]))))))))))

;; LL(1) parsing

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
    (->> (keys productions)
         (map (juxt identity (make-first-set productions nullable?)))
         (into {}))))
