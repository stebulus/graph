# amotlpaa.graph

A simple library for depth-first search and related algorithms.

The [`amotlpaa.df`](src/amotlpaa/df.clj) namespace provides an abstract
notion of a cursor in a depth-first search, and tools for manipulating
such cursors.  The [`amotlpaa.graph`](src/amotlpaa/graph.clj)
namespace provides a simple adjacency-list representation of graphs
and some basic algorithms for them, making use of `df`.  (See the
implementation of `scc-map`, for example.)
