(ns gmwbot.df
  (:refer-clojure :exclude [reduce])
  (:require [clojure.core :as core]))

(defprotocol DepthFirstCursor
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

(declare dfc)
(defrecord StackDFC [children stack]
  DepthFirstCursor
  (down [this]
    (some->> (seq (children (current this)))
             (conj stack)
             (StackDFC. children)))
  (across [this]
    (some->> (next (peek stack))
             (conj (pop stack))
             (StackDFC. children)))
  (up [this]
    (let [s (pop stack)]
      (when-not (empty? s)
        (StackDFC. children s))))
  (current [this]
    (first (peek stack)))
  (reroot [this]
    (dfc children (current this))))
(defn dfc
  "A depth-first cursor in graph, starting at node start.  The graph
  is a callable taking a node as an argument and returning a seq of
  that node's children."
  [graph start]
  (StackDFC. graph [[start]]))

(defn skip
  "Iteratively call move on cursor as long as the cursor satisfies
  pred.  Returns the first cursor that doesn't satisfy pred, or nil
  if none is found."
  [move pred cursor]
  (->> (iterate move cursor)
       (take-while some?)
       (drop-while pred)
       (first)))
(defn scan
  "Iteratively call move on cursor until the cursor satisfies pred.
  Returns the first cursor that satisfies pred, or nil if none
  is found."
  [move pred cursor]
  (skip move #(not (pred %)) cursor))

(defn loop?
  "Whether the current node was reached by traversing a loop."
  [cursor]
  (let [curr (current cursor)]
    (->> (iterate up cursor)
         (take-while some?)
         (drop 1)
         (map current)
         (some #(= curr %)))))

(defprotocol Wrapper
  "An object which wraps another object."
  (unwrap [this] "Returns the object wrapped by this one."))

(declare doeach-move)
(defrecord DoEachDFC [f cursor]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this]
    (doeach-move down f cursor))
  (across [this]
    (doeach-move across f cursor))
  (up [this]
    (doeach-move up f cursor))
  (current [this]
    (current cursor))
  (reroot [this]
    (doeach-move reroot f cursor)))
(defn- doeach-move [move f cursor]
  (when-let [s (move cursor)]
    (f s)
    (DoEachDFC. f s)))
(defn doeach
  "A cursor wrapper which evaluates (f cursor) at every node and
  discards the result."
  [f cursor]
  (doeach-move identity f cursor))

(defmacro never
  "A cursor wrapper which asserts (not (pred cursor)) at every node."
  [pred cursor]
  `(doeach #(assert (not (~pred %))) ~cursor))

(declare seen-move record-seen)
(defrecord SeenDFC [seen cursor]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this]
    (seen-move down seen cursor))
  (across [this]
    (seen-move across seen cursor))
  (up [this]
    (seen-move up seen cursor))
  (current [this]
    (current cursor))
  (reroot [this]
    (record-seen (reroot cursor))))
(defn- seen-move [move seen cursor]
  (some->> (move cursor)
           (SeenDFC. (conj seen (current cursor)))))
(defn record-seen
  "A cursor wrapper which remembers all nodes seen.  To find out
  if the current node has been seen before, use (seen? cursor).
  Rerooting this cursor yields a cursor which has seen nothing."
  [cursor]
  (SeenDFC. #{} cursor))
(defn seen?
  "Whether the current node of the cursor seenc has been seen before.
  The cursor must be of the type returned by record-seen."
  [seenc]
  (contains? (.seen seenc) (current seenc)))

(declare prune)
(defrecord PrunedDFC [pred cursor]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this]
    (some->> (down cursor)
             (skip across pred)
             (PrunedDFC. pred)))
  (across [this]
    (some->> (across cursor)
             (skip across pred)
             (PrunedDFC. pred)))
  (up [this]
    (some->> (up cursor)
             (PrunedDFC. pred)))
  (current [this]
    (current cursor))
  (reroot [this]
    (prune pred (reroot cursor))))
(defn prune
  "A cursor wrapper which omits cursors satisfying pred (and therefore
  also skips the subtrees under them).  Note that pred takes the
  cursor as an argument, not the node; thus pred may depend on the
  current node's neighbourhood.  For example, if pred is
  #(some->> (down %) (current) (= :x)), then nodes which have :x as
  their first child will be pruned."
  [pred cursor]
  (PrunedDFC. pred cursor))

(defn prune-seen [cursor]
  (prune seen? (record-seen cursor)))

(declare stepper-move stepper)
(defrecord StepperDFC [cursor inbound]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this]
    (stepper-move this down true))
  (across [this]
    (stepper-move this across true))
  (up [this]
    (stepper-move this up false))
  (current [this]
    (current cursor))
  (reroot [this]
    (stepper (reroot cursor))))
(defn- stepper-move [stepc move inbound]
  (when-let [s (move (.cursor stepc))]
    (StepperDFC. s inbound)))
(defn stepper [cursor]
  (StepperDFC. cursor true))
(defn inbound? [stepc]
  (.inbound stepc))
(defn step [stepc]
  (if (inbound? stepc)
    (or (down stepc) (stepper-move stepc identity false))
    (or (across stepc) (up stepc))))
(defn step-over [stepc]
  (stepper-move stepc identity false))

(defn preorder-tree [cursor]
  "Returns a lazy seq of nodes as by a preorder traversal of cursor.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; if there is a loop in
  the graph, the seq will get trapped in it.  (As the name suggests,
  this is an appropriate function if you know the graph is a tree.)"
  (->> (stepper cursor)
       (iterate step)
       (take-while some?)
       (filter inbound?)
       (map current)))
(defn preorder [cursor]
  "Returns a lazy seq of nodes as by a preorder traversal of cursor.
  Skips nodes that have appeared before."
  (preorder-tree (prune-seen cursor)))

(defn postorder-tree [cursor]
  "Returns a lazy seq of nodes as by a postorder traversal of cursor.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; attempting to realize
  any element of a loop will hang.  (As the name suggests, this is
  an appropriate function if you know the graph is a tree.)"
  (->> (stepper cursor)
       (iterate step)
       (take-while some?)
       (filter #(not (inbound? %)))
       (map current)))
(defn postorder [cursor]
  "Returns a lazy seq of nodes as by a postorder traversal of cursor.
  Skips nodes that have appeared before."
  (postorder-tree (prune-seen cursor)))

(defn reduce [f initf cursor]
  (loop [stack []
         cursor (->> (reroot cursor)
                     (never loop?)
                     (stepper))]
    (if (inbound? cursor)
      (let [init (initf cursor)]
        (if (reduced? init)
          (recur (conj stack @init)
                 (step-over cursor))
          (recur (conj stack init)
                 (step cursor))))
      (let [top (peek stack)
            stack (pop stack)]
        (if (empty? stack)
          top
          (let [val (f (peek stack) top)]
            (if (reduced? val)
              (recur (conj (pop stack) @val)
                     (up cursor))
              (recur (conj (pop stack) val)
                     (step cursor)))))))))
