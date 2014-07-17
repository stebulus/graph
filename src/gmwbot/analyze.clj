(ns gmwbot.analyze
  (:use clojure.set)
  (:require [gmwbot.df :as df]))

;; A few graph algorithms

(defn scc-map
  "Returns a map whose keys are the nodes of graph and whose values
  are the strongly connected components to which those nodes belong,
  as sets of nodes.  Uses Kosaraju's algorithm."
  [graph]
  (let [transpose (apply merge-with
                         into
                         (for [[k vs] graph v vs] {v [k]}))]
    (reduce (fn [vscc v]
              (if (contains? vscc v)
                vscc
                (let [newscc (->> (df/dfc transpose v)
                                  (df/prune vscc)
                                  (df/preorder)
                                  (into #{}))]
                  (into vscc
                        (for [node newscc] [node newscc])))))
            {}
            (->> (map #(df/dfc graph %) (keys graph))
                 (df/as-siblings)
                 (df/postorder)
                 (reverse)))))
(defn scc
  "Returns a set of the strongly connected components of graph,
  which are sets of nodes."
  [graph]
  (into #{} (vals (scc-map graph))))

(defn vertex-map
  "Returns a graph which has an edge from (f a) to (f b) whenever
  the given graph has an edge from a to b.  Duplicate edges are
  not removed."
  [f graph]
  (apply merge-with
         into
         (for [[k vs] graph v vs]
              {(f k) [(f v)]})))
(defn unique-edges
  "Returns a graph with the same nodes as the given one, but without
  duplicate edges."
  [graph]
  (into {} (for [[k vs] graph] [k (set vs)])))
(defn remove-loops
  "Returns a graph without any edges from a node to itself."
  [graph]
  (into {} (for [[k vs] graph] [k (into [] (remove #(= k %) vs))])))
(defn simplify [graph]
  (remove-loops (unique-edges graph)))
(defn condense
  "Returns a graph whose nodes are the strongly connected components
  of the given graph (as sets of nodes), with a (single) edge from
  one component to another when any node in the first component has
  an edge to any node of the second."
  [graph]
  (simplify (vertex-map (scc-map graph) graph)))

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

(defn- empty-graph [vertices]
  (zipmap vertices (repeat [])))
(defn- into-graph [graph edges]
  (if-some [s (seq edges)]
    (let [[a b] (first edges)]
      (recur (assoc graph a (conj (get graph a []) b))
             (rest edges)))
    graph))
(defn follow-deps
  ([productions]
    (let [nullable? (make-nullable? productions)]
      (follow-deps productions
                   nullable?
                   (make-first-set productions nullable?))))
  ([productions nullable? first-set]
    (into-graph (empty-graph (map first productions))
                (for [[lhs rhses] productions
                      rhs rhses
                      sym (take-until #(not (nullable? %))
                                      (reverse rhs))
                      :when (contains? productions sym)]
                  [sym lhs]))))
(defn- base-follow [rhs nullable? first-set]
  (let [revrhs (reverse rhs)]
    (map (fn [a b] [a b])
         revrhs
         (reductions (fn [fol sym]
                       (let [fir (first-set sym)]
                         (if (nullable? sym)
                           (union fol fir)
                           fir)))
                     #{}
                     revrhs))))
(defn follow-sets
  [productions nullable? first-set]
  (let [deps (follow-deps productions nullable? first-set)
        sccm (scc-map deps)
        cdeps (simplify (vertex-map sccm deps))
        base (reduce (fn [m [k v]]
                       (merge-with union m {k v}))
                     {}
                     (->> (for [rhses (vals productions) rhs rhses] rhs)
                          (mapcat #(base-follow % nullable? first-set))
                          (filter (fn [[k v]] (contains? productions k)))
                          (map (fn [[k v]] [(sccm k) v]))))]
    (into {}
          (for [[component folset]
                 (reduce #(df/memo-reduce
                            %1
                            union
                            base
                            (df/dfc cdeps %2))
                         {}
                         (keys cdeps))
                v component]
            [v folset]))))
