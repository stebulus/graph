(ns gmwbot.dfs
  (:refer-clojure :exclude [reduce])
  (:require [clojure.core :as core]))

(defprotocol DepthFirstSearch
  "A cursor for a depth-first traversal of a directed graph.  These
  cursors are immutable; navigation functions return a new cursor,
  and the old cursor remains valid.  A cursor is only guaranteed
  to be able to reach nodes which would be visited after it in a
  depth-first traversal.  For example, consider the graph
        A -> B C D
        B -> X Y
  (that is, a graph with nodes A, B, C, D, X, Y, with arcs from A to B,
  to C, and to D, and arcs from B to X and to Y).  If we start with
  a cursor at A, move down to B, then across to C, the resulting
  cursor at C is not guaranteed to be able to access B, X, or Y.
  (We might try to move up to A and then back down; the move up to A
  will succeed, because A would normally be visited again after its
  children had been traversed, but the move down from A again might
  fail.)  Note that, consequently, if we retain only a reference to
  the cursor at C, the nodes B, X, and Y could be garbage-collected."
  (down [this]
    "Returns a cursor at the first (leftmost) child of the current node,
    or nil if the current node has no children.")
  (across [this]
    "Returns a cursor at the next (to the right) sibling of the
    current node, or nil if this node has no further siblings.
    The resulting cursor may not be able to access the current node
    or any previous siblings.")
  (up [this]
    "Returns a cursor at the parent of this node, or nil if this node
    has no parent.  The resulting cursor may not be able to access
    the current node or any of its siblings.")
  (current [this]
    "The current node.")
  (reroot [this]
    "Returns a cursor at the current node, but which cannot move up.
    Any state associated with the path by which this cursor reached
    the current node is omitted from the new cursor.  A caller can use
    this function to declare that it has no further interest in the
    ancestors of the current node (and so, for example, they could
    be garbage-collected).  For least surprise, the cursor obtained
    by reroot should usually be the same as a newly constructed one
    at the same location."))

(declare dfs)
(defrecord StackDFS [children stack]
  DepthFirstSearch
  (down [this]
    (some->> (seq (children (first (peek stack))))
             (conj stack)
             (StackDFS. children)))
  (across [this]
    (some->> (next (peek stack))
             (conj (pop stack))
             (StackDFS. children)))
  (up [this]
    (let [s (pop stack)]
      (when-not (empty? s)
        (StackDFS. children s))))
  (current [this]
    (first (peek stack)))
  (reroot [this]
    (dfs children (current this))))
(defn dfs [graph start]
  (StackDFS. graph [[start]]))

(defn skip [move pred search]
  (->> (iterate move search)
       (take-while some?)
       (drop-while pred)
       (first)))
(defn scan [move pred search]
  (skip move #(not (pred %)) search))

(defn loop? [search]
  (let [curr (current search)]
    (->> (iterate up search)
         (take-while some?)
         (drop 1)
         (map current)
         (some #(= curr %)))))

(defprotocol Wrapper
  (unwrap [this]))

(declare doeach-move)
(defrecord DoEachDFS [f search]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (doeach-move down f search))
  (across [this]
    (doeach-move across f search))
  (up [this]
    (doeach-move up f search))
  (current [this]
    (current search))
  (reroot [this]
    (doeach-move reroot f search)))
(defn- doeach-move [move f search]
  (when-let [s (move search)]
    (f s)
    (DoEachDFS. f s)))
(defn doeach [f search]
  (doeach-move identity f search))

(defmacro never [pred search]
  `(doeach #(assert (not (~pred %))) ~search))

(declare seen-move record-seen)
(defrecord SeenDFS [seen search]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (seen-move down seen search))
  (across [this]
    (seen-move across seen search))
  (up [this]
    (seen-move up seen search))
  (current [this]
    (current search))
  (reroot [this]
    (record-seen (reroot search))))
(defn- seen-move [move seen search]
  (some->> (move search)
           (SeenDFS. (conj seen (current search)))))
(defn record-seen [search]
  (SeenDFS. #{} search))
(defn seen? [seendfs]
  (contains? (.seen seendfs) (current seendfs)))

(declare prune)
(defrecord PrunedDFS [pred search]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (some->> (down search)
             (skip across pred)
             (PrunedDFS. pred)))
  (across [this]
    (some->> (across search)
             (skip across pred)
             (PrunedDFS. pred)))
  (up [this]
    (some->> (up search)
             (PrunedDFS. pred)))
  (current [this]
    (current search))
  (reroot [this]
    (prune pred (reroot search))))
(defn prune [pred search]
  (PrunedDFS. pred search))

(defn prune-seen [search]
  (prune seen? (record-seen search)))

(declare stepper-move stepper)
(defrecord StepperDFS [search inbound]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (stepper-move this down true))
  (across [this]
    (stepper-move this across true))
  (up [this]
    (stepper-move this up false))
  (current [this]
    (current search))
  (reroot [this]
    (stepper (reroot search))))
(defn- stepper-move [stepdfs move inbound]
  (when-let [s (move (.search stepdfs))]
    (StepperDFS. s inbound)))
(defn stepper [search]
  (StepperDFS. search true))
(defn inbound? [stepdfs]
  (.inbound stepdfs))
(defn step [stepdfs]
  (if (inbound? stepdfs)
    (or (down stepdfs) (stepper-move stepdfs identity false))
    (or (across stepdfs) (up stepdfs))))
(defn step-over [stepdfs]
  (stepper-move stepdfs identity false))

(defn preorder-tree [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; if there is a loop in
  the graph, the seq will get trapped in it.  (As the name suggests,
  this is an appropriate function if you know the graph is a tree.)"
  (->> (stepper search)
       (iterate step)
       (take-while some?)
       (filter inbound?)
       (map current)))
(defn preorder [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  Skips nodes that have appeared before."
  (preorder-tree (prune-seen search)))

(defn postorder-tree [search]
  "Returns a lazy seq of nodes as by a postorder traversal of search.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; attempting to realize
  any element of a loop will hang.  (As the name suggests, this is
  an appropriate function if you know the graph is a tree.)"
  (->> (stepper search)
       (iterate step)
       (take-while some?)
       (filter #(not (inbound? %)))
       (map current)))
(defn postorder [search]
  "Returns a lazy seq of nodes as by a postorder traversal of search.
  Skips nodes that have appeared before."
  (postorder-tree (prune-seen search)))

(defn reduce [f initf search]
  (loop [stack []
         search (->> (reroot search)
                     (never loop?)
                     (stepper))]
    (if (inbound? search)
      (let [init (initf search)]
        (if (reduced? init)
          (recur (conj stack @init)
                 (step-over search))
          (recur (conj stack init)
                 (step search))))
      (let [top (peek stack)
            stack (pop stack)]
        (if (empty? stack)
          top
          (let [val (f (peek stack) top)]
            (if (reduced? val)
              (recur (conj (pop stack) @val)
                     (up search))
              (recur (conj (pop stack) val)
                     (step search)))))))))
