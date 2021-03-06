(ns amotlpaa.df
  (:refer-clojure :exclude [map reduce reductions])
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
  the cursor at C, the nodes B, X, and Y could be garbage-collected.
  All implementations of navigation functions are encouraged to be
  as lazy as possible; for example, the children of a node should if
  possible not be computed until down is called."
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
    "Returns a cursor at the current node, but which cannot move
    up or across from that node.  Any state associated with the
    path by which this cursor reached the current node is omitted
    from the new cursor.  A caller can use this function to declare
    that it has no further interest in the ancestors or following
    siblings of the current node (and so, for example, they could
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

(defrecord SiblingSeqDFC [cursor depth siblings]
  DepthFirstCursor
  (down [this]
    (when-let [child (down cursor)]
      (SiblingSeqDFC. child (inc depth) siblings)))
  (across [this]
    (if-some [sib (across cursor)]
      (SiblingSeqDFC. sib depth siblings)
      (when (zero? depth)
        (when-first [sib siblings]
          (SiblingSeqDFC. sib depth (rest siblings))))))
  (up [this]
    (if (zero? depth)
      nil
      (when-let [parent (up cursor)]
        (SiblingSeqDFC. parent (dec depth) siblings))))
  (current [this]
    (current cursor))
  (reroot [this]
    (reroot cursor)))
(defn as-siblings [cursors]
  "A cursor over a graph in which the current nodes of the cursors
  in the given sequence, and their siblings, are siblings."
  (when-first [cursor cursors]
    (SiblingSeqDFC. cursor 0 (rest cursors))))

(defn skip
  "Iteratively call move on cursor as long as the node satisfies
  pred.  Returns a cursor at the first node that doesn't satisfy
  pred (possibly the cursor passed as an argument), or nil if none
  is found."
  [move pred cursor]
  (->> (iterate move cursor)
       (take-while some?)
       (drop-while #(pred (current %)))
       (first)))
(defn scan
  "Iteratively call move on cursor until the cursor satisfies pred.
  Returns the first cursor that satisfies pred (possibly the cursor
  passed as an argument), or nil if none is found."
  [move pred cursor]
  (skip move #(not (pred %)) cursor))

(defn loop?
  "Whether the current node was reached by traversing a loop."
  [cursor]
  (let [curr (current cursor)]
    (->> (iterate up cursor)
         (take-while some?)
         (drop 1)
         (core/map current)
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

(defn always
  "A cursor wrapper which asserts (pred cursor) at every node."
  ([pred cursor]
    (always pred nil cursor))
  ([pred msg cursor]
    (doeach #(assert (pred %)
                     (if (some? msg)
                       (print-str msg %)
                       (print-str %)))
            cursor)))
(defn never
  "A cursor wrapper which asserts (not (pred cursor)) at every node."
  ([pred cursor]
    (always #(not (pred %)) cursor))
  ([pred msg cursor]
    (always #(not (pred %)) msg cursor)))

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
  "A cursor wrapper which omits nodes satisfying pred (and therefore
  also skips the subtrees under them)."
  [pred cursor]
  (PrunedDFC. pred cursor))

(declare curcur)
(defrecord CurcurDFC [cursor]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this] (some-> (down cursor) (curcur)))
  (across [this] (some-> (across cursor) (curcur)))
  (up [this] (some-> (up cursor) (curcur)))
  (current [this] cursor)
  (reroot [this] (curcur (reroot cursor))))
(defn curcur
  "A cursor wrapper representing a graph with the same structure as
  the underlying cursor, but whose nodes are the cursors themselves.
  Useful especially when combined with functions such as prune, skip,
  scan, and map, which apply functions to the nodes of a given cursor.
  For example,
      (->> (curcur cursor)
           (prune #(some->> (down %) (current) (= :x)))
           (map current))
  prunes nodes of cursor which have the node :x as first child."
  [cursor]
  (CurcurDFC. cursor))

(declare map)
(defrecord MapDFC [f cursor]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this] (some->> (down cursor) (map f)))
  (across [this] (some->> (across cursor) (map f)))
  (up [this] (some->> (up cursor) (map f)))
  (current [this] (f (current cursor)))
  (reroot [this] (map f (reroot cursor))))
(defn map
  "A cursor whose nodes are the values of f applied to the nodes of
  the underlying cursor."
  [f cursor]
  (MapDFC. f cursor))

(defn prune-seen
  "A cursor wrapper which omits nodes already seen."
  [cursor]
  (map current (prune seen? (curcur (record-seen cursor)))))

(defprotocol Directed
  (inbound? [this]
    "Whether the current direction of the cursor is inbound.  See stepper.")
  (step-over [this]
    "Skip the descendants of the current node of the given stepping
    cursor, thus becoming outbound on the current node.  See stepper."))

(declare stepper-move stepper)
(defrecord StepperDFC [cursor inbound]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this]
    (stepper-move down true cursor))
  (across [this]
    (stepper-move across true cursor))
  (up [this]
    (stepper-move up false cursor))
  (current [this]
    (current cursor))
  (reroot [this]
    (stepper (reroot cursor)))
  Directed
  (inbound? [this]
    (.inbound this))
  (step-over [this]
    (stepper-move identity false cursor)))
(defn- stepper-move [move inbound cursor]
  (when-let [s (move cursor)]
    (StepperDFC. s inbound)))
(defn stepper
  "A cursor wrapper which keeps track of the current 'direction'.
  In a depth-first traversal, a node is typically visited twice,
  once 'inbound', before its descendants, and once 'outbound', after
  its descendants; (inbound? c) reports the direction, and (step c)
  returns a cursor for the next state of the traversal.  For example,
  consider the graph
        A -> B C
  (that is, a graph with nodes A, B, C, with arcs from A to B and
  to C).  The sequence of stepping cursor states for traversing this
  graph from A is
        A inbound
        B inbound
        B outbound
        C inbound
        C outbound
        A outbound
  Calling step will iterate through these states.  To skip the
  descendants of the current node, call step-over.  To skip the
  descendants of the current node and the outbound state of the current
  node, call step-over and then step.  To skip any further siblings
  of the current node (as well as, if inbound, the descendants of the
  current node and the outbound state of the current node), call up.
  Rerooting this cursor yields an inbound cursor."
  [cursor]
  (StepperDFC. cursor true))
(defn step
  "Advance the stepping cursor.  See stepper."
  [stepc]
  (if (inbound? stepc)
    (or (down stepc) (step-over stepc))
    (or (across stepc) (up stepc))))

(defn preorder-tree
  "Returns a lazy seq of nodes as by a preorder traversal of cursor.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; if there is a loop in
  the graph, the seq will get trapped in it.  (As the name suggests,
  this is an appropriate function if you know the graph is a tree.)
  The traversal is not constrained to the subtree rooted at the
  current node; for that, use (preorder-tree (reroot cursor))."
  [cursor]
  (->> (stepper cursor)
       (iterate step)
       (take-while some?)
       (filter inbound?)
       (core/map current)))
(defn preorder
  "Returns a lazy seq of nodes as by a preorder traversal of cursor.
  Skips nodes that have appeared before.  The traversal is not
  constrained to the subtree rooted at the current node; for that,
  use (preorder (reroot cursor))."
  [cursor]
  (preorder-tree (prune-seen cursor)))

(defn postorder-tree
  "Returns a lazy seq of nodes as by a postorder traversal of cursor.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; attempting to realize
  any element of a loop will hang.  (As the name suggests, this
  is an appropriate function if you know the graph is a tree.)
  The traversal is not constrained to the subtree rooted at the
  current node; for that, use (postorder-tree (reroot cursor))."
  [cursor]
  (->> (stepper cursor)
       (iterate step)
       (take-while some?)
       (filter #(not (inbound? %)))
       (core/map current)))
(defn postorder
  "Returns a lazy seq of nodes as by a postorder traversal of cursor.
  Skips nodes that have appeared before.  The traversal is not
  constrained to the subtree rooted at the current node; for that,
  use (postorder (reroot cursor))."
  [cursor]
  (postorder-tree (prune-seen cursor)))

(defn prune-children
  "A cursor wrapper which omits the children of the current node."
  [cursor]
  (reify
    Wrapper
    (unwrap [this] cursor)
    DepthFirstCursor
    (down [this] nil)
    (across [this] (across cursor))
    (up [this] (up cursor))
    (current [this] (current cursor))
    (reroot [this] (prune-children (reroot cursor)))
    Directed
    (inbound? [this] (inbound? cursor))
    (step-over [this] (step-over cursor))))
(defn prune-siblings
  "A cursor wrapper which omits the following siblings of the current node."
  ([cursor]
    (prune-siblings 0 cursor))
  ([depth cursor]
    (reify
      Wrapper
      (unwrap [this] cursor)
      DepthFirstCursor
      (down [this] (prune-siblings (inc depth) (down cursor)))
      (across [this]
        (if (zero? depth)
          nil
          (prune-siblings depth (across cursor))))
      (up [this]
        (let [parent (up cursor)]
          (if (zero? depth)
            parent
            (prune-siblings (dec depth) parent))))
      (current [this] (current cursor))
      (reroot [this] (reroot cursor))
      Directed
      (inbound? [this] (inbound? cursor))
      (step-over [this] (prune-siblings depth (step-over cursor))))))

(declare reducer-move)
(defrecord ReducerDFC [inf outf state cursor]
  Wrapper
  (unwrap [this] cursor)
  DepthFirstCursor
  (down [this] (reducer-move inf outf state down cursor))
  (across [this] (reducer-move inf outf state across cursor))
  (up [this] (reducer-move inf outf state up cursor))
  (current [this] state)
  (reroot [this] (ReducerDFC. inf outf state (reroot cursor)))
  Directed
  (inbound? [this] (inbound? cursor))
  (step-over [this] (reducer-move inf outf state step-over cursor)))
(defn- reducer-move [inf outf state move cursor]
  (when-let [cursor (move cursor)]
    (let [combineresult ((if (inbound? cursor) inf outf)
                          state
                          (current cursor))]
      (if (reduced? combineresult)
        (ReducerDFC. inf
                     outf
                     @combineresult
                     (if (inbound? cursor)
                       (prune-children cursor)
                       (prune-siblings cursor)))
        (ReducerDFC. inf
                     outf
                     combineresult
                     cursor)))))
(defn reductions
  "A cursor over a graph with the same structure as the given cursor,
  whose nodes are states computed by combining the previous state with
  newly visited nodes of the given cursor.  The initial value of the
  state is init; when a node is visited inbound, the state becomes
  the value of (inf state node); when a node is visited outbound,
  the state becomes the value of (outf state node).  When reductions
  is called, the state will immediately be updated from init as if
  visiting the current node inbound.  For example,
      (->> (reductions inf outf init (dfc {1 [2 3] 2 [4 5]} 1))
           (iterate step)
           (take-while some?)
           (last)
           (current))
  will return the same value as
      (-> init (inf 1) (inf 2) (inf 4) (outf 4) (inf 5)
          (outf 5) (outf 2) (inf 3) (outf 3) (outf 1))
  If inf returns a reduced value (see clojure.core/reduced), then
  the children of the current node will be pruned; if outf returns
  a reduced value, the following siblings of the current node will
  be pruned.  (In both cases, the actual new state is the unwrapped
  value, not the reduced value.)  Note that calling across directly
  on this cursor when it is inbound on the current node will pass
  directly to the inbound state of the following sibling, skipping the
  outbound state of the current node, and so outf will not be called;
  thus it's usually advisable to traverse this cursor using step.
  This cursor does not detect loops or treat previously seen nodes
  in any way different from novel nodes."
  [inf outf init cursor]
  (reducer-move inf outf init stepper cursor))

(defn- percolate-reduced [x f]
  (if (reduced? x)
    (reduced (f @x))
    (f x)))
(defn reduce
  "Reduce the subtree under the current node of cursor.  The result
  is pretty much the same as
      (clojure.core/reduce f (initf (current cursor)) child-vals)
  where child-vals is a lazy sequence of the values of the children of
  the current node, which are computed in the same way.  For example,
      (amotlpaa.df/reduce + identity (dfc {1 [2 3] 2 [4 5]} 1))
  computes 15 as if by
      (+ (+ 1 (+ (+ 2 4) 5)) 3)
  (Since + is associative, this example would be more efficiently
  computed with
      (clojure.core/reduce + (preorder ...))
  because this version doesn't need to maintain its own stack.)
  Unlike with clojure.core/reduce, the initial value (function) may
  not be omitted.  The combining function f is only ever called with
  two arguments; if a node has no children, its initial value from
  initf will be used and f will not be called.  Short-circuiting
  occurs in two ways: first, if initf returns a reduced value (see
  clojure.core/reduced), then the children of the current node will
  not be visited, nor their values computed; second, if f returns
  a reduced value, then the following siblings of the current node
  will not be visited, nor their values computed.  To short-circuit
  the entirety of the rest of the tree, one technique is to return
  doubly-reduced values and check for a reduced second argument in f;
  see amotlpaa.df-test/super-reduce for an example.  If the value of a
  node is found to depend on itself (because there is a loop in the
  graph being traversed which is not avoided by short-circuiting),
  an AssertionError will be thrown.  The value of a node will be
  recomputed every time it is encountered; to remember and reuse
  previously computed values, use memo-reduce."
  [f initf cursor]
  ; We could implement this by calling core/reduce as in the docstring,
  ; but it'd be a bit tricky to wrangle the cursors correctly.
  ; The following implementation only ever handles one cursor at a
  ; time, and simply advances it from node to node as usual.
  (->> (reductions (fn [stack node]
                     (percolate-reduced (initf node) #(conj stack %)))
                   (fn [stack node]
                     (let [popped (pop stack)]
                       (if (empty? popped)
                         stack
                         (percolate-reduced (f (peek popped) (peek stack))
                                            #(conj (pop popped) %)))))
                   []
                   (never loop? (reroot cursor)))
       (iterate step)
       (take-while some?)
       (last)
       (current)
       (peek)))

(defn memo-reduce
  "Reduce the subtree under the current node of cursor.  Returns a
  map from node to value, with entries for the initial node of the
  cursor and all descendants whose values were computed along the way.
  If a node is encountered more than once (on different branches, not
  as its own descendant), the value computed on the first encounter
  will be reused, and its subtree will not be traversed again.
  Otherwise, the computation is the same as that performed by reduce
  (q.v.); in particular, f and initf may return reduced values to
  cause short-circuiting, and if the value of a node is found to
  depend on itself, an AssertionError will be thrown."
  ([f initf cursor]
    (memo-reduce {} f initf cursor))
  ([memo f initf cursor]
    (->> (reductions (fn [[stack memo] node]
                       (let [newstack (conj stack node)]
                         (if (contains? memo node)
                           (reduced [newstack memo])
                           (percolate-reduced (initf node)
                                              #(list newstack (assoc memo node %))))))
                     (fn [[stack memo] node]
                       (let [newstack (pop stack)]
                         (if (empty? newstack)
                           memo
                           (let [parent-node (peek newstack)]
                             (percolate-reduced (f (get memo parent-node) (get memo node))
                                                #(list newstack (assoc memo parent-node %)))))))
                     [[] memo]
                     (never loop? (reroot cursor)))
         (iterate step)
         (take-while some?)
         (last)
         (current))))
