(ns gmwbot.graph
  (:refer-clojure :exclude [empty into map])
  (:require [clojure.core :as core]
            [gmwbot.df :as df]))

;; A few graph algorithms

(defn empty [vertices]
  (zipmap vertices (repeat [])))
(defn into [graph edges]
  (if-some [s (seq edges)]
    (let [[a b] (first edges)]
      (recur (assoc graph a (conj (get graph a []) b))
             (rest edges)))
    graph))
(defn edges [graph]
  (for [[k vs] graph v vs] [k v]))
(defn vertices-with-duplicates [graph]
  (concat (keys graph) (for [[k vs] graph v vs] v)))
(defn vertices [graph]
  (set (vertices-with-duplicates graph)))
(defn transpose [graph]
  (->> (edges graph)
       (core/map reverse)
       (into (empty (vertices-with-duplicates graph)))))

(defn scc-map
  "Returns a map whose keys are the nodes of graph and whose values
  are the strongly connected components to which those nodes belong,
  as sets of nodes.  Uses Kosaraju's algorithm."
  [graph]
  (let [t (transpose graph)]
    (reduce (fn [vscc v]
              (if (contains? vscc v)
                vscc
                (let [newscc (->> (df/dfc t v)
                                  (df/prune vscc)
                                  (df/preorder)
                                  (core/into #{}))]
                  (core/into vscc
                             (for [node newscc] [node newscc])))))
            {}
            (->> (core/map #(df/dfc graph %) (keys graph))
                 (df/as-siblings)
                 (df/postorder)
                 (reverse)))))
(defn scc
  "Returns a set of the strongly connected components of graph,
  which are sets of nodes."
  [graph]
  (core/into #{} (vals (scc-map graph))))

(defn map
  "Returns a graph which has an edge from (f a) to (f b) whenever
  the given graph has an edge from a to b.  Duplicate edges are
  not removed."
  [f graph]
  (let [fm (core/into {} (for [v (vertices graph)] [v (f v)]))]
    (->> (edges graph)
         (core/map (fn [[a b]] [(fm a) (fm b)]))
         (into (empty (vals fm))))))
(defn unique-edges
  "Returns a graph with the same nodes as the given one, but without
  duplicate edges."
  [graph]
  (into (empty (vertices-with-duplicates graph))
        (set (edges graph))))
(defn remove-loops
  "Returns a graph without any edges from a node to itself."
  [graph]
  (->> (edges graph)
       (filter (fn [[a b]] (not= a b)))
       (into (empty (vertices-with-duplicates graph)))))
(defn simplify [graph]
  (remove-loops (unique-edges graph)))
(defn condense
  "Returns a graph whose nodes are the strongly connected components
  of the given graph (as sets of nodes), with a (single) edge from
  one component to another when any node in the first component has
  an edge to any node of the second."
  [graph]
  (simplify (map (scc-map graph) graph)))
