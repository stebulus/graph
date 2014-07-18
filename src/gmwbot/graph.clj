(ns gmwbot.graph
  (:refer-clojure :exclude [empty into map])
  (:require [clojure.core :as core]
            [gmwbot.df :as df]))

(defn empty
  "Returns a graph with the given vertices and no edges."
  [vertices]
  (zipmap vertices (repeat [])))
(defn into
  "Returns the given graph with the given edges added."
  [graph edges]
  (if-some [s (seq edges)]
    (let [[a b] (first edges)]
      (recur (assoc graph a (conj (get graph a []) b))
             (rest edges)))
    graph))
(defn edges
  "Returns a lazy sequence of the edges of the given graph."
  [graph]
  (for [[k vs] graph v vs] [k v]))
(defn vertices-with-duplicates
  "Returns a lazy sequence of the vertices of the graph, almost
  certainly with duplicates."
  [graph]
  (concat (keys graph) (for [[k vs] graph v vs] v)))
(defn vertices
  "Returns a set of the vertices of graph."
  [graph]
  (set (vertices-with-duplicates graph)))

(defn map
  "Returns a graph whose vertices are the values (f v), where v
  is a vertex of the given graph, and which has an edge from from
  (f a) to (f b) whenever the given graph has an edge from a to b.
  Note that if f is not one-to-one, duplicate edges may be produced."
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
(defn simplify
  "Returns a simple version of graph, that is, one without duplicate
  edges and without edges from a vertex to itself."
  [graph]
  (remove-loops (unique-edges graph)))

(defn transpose
  "Returns a graph with the same vertices as graph, but with all
  edges going the opposite direction."
  [graph]
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
            (->> (core/map #(df/dfc graph %)
                           (vertices-with-duplicates graph))
                 (df/as-siblings)
                 (df/postorder)
                 (reverse)))))
(defn scc
  "Returns a set of the strongly connected components of graph,
  which are sets of nodes."
  [graph]
  (core/into #{} (vals (scc-map graph))))
(defn condense
  "Returns a graph whose nodes are the strongly connected components
  of the given graph (as sets of nodes), with a (single) edge from
  one component to another when any node in the first component has
  an edge to any node of the second."
  [graph]
  (simplify (map (scc-map graph) graph)))
