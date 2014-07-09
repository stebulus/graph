(ns gmwbot.dfs)
(defprotocol DepthFirstSearch
  (down [this])
  (across [this])
  (up [this])
  (last-edge [this]))
(defrecord StackDFS [stack children]
  DepthFirstSearch
  (down [this]
    (when-let [s (seq (children (first (peek stack))))]
      (StackDFS. (conj stack s) children)))
  (across [this]
    (when-let [s (next (peek stack))]
      (StackDFS. (conj (pop stack) s) children)))
  (up [this]
    (let [s (pop stack)]
      (when-not (empty? s)
        (StackDFS. s children))))
  (last-edge [this]
    [(first (peek (pop stack)))
     (first (peek stack))]))
(defn dfs [graph start]
  (StackDFS. [[start]] graph))
(defn scan-across [search pred]
  (if (pred (second (last-edge search)))
    search
    (when-let [s (across search)]
      (recur s pred))))
(defn scan-children [search pred]
  (some-> (down search)
          (scan-across pred)))
(defrecord PrunedDFS [search seen]
  DepthFirstSearch
  (down [this]
    (when-let [s (scan-children search #(not (contains? seen %)))]
      (PrunedDFS. s (conj seen (second (last-edge s))))))
  (across [this]
    (when-let [s (scan-across search #(not (contains? seen %)))]
      (PrunedDFS. s (conj seen (second (last-edge s))))))
  (up [this]
    (when-let [s (up search)]
      (PrunedDFS. s seen)))
  (last-edge [this]
    (last-edge search)))
(defn prune-seen [search]
  (PrunedDFS. search #{(second (last-edge search))}))

(defn step [search]
  (if-let [s (down search)]
    s
    (loop [s search]
      (if-let [s (across s)]
        s
        (when-let [s (up s)]
          (recur s))))))

(defn preorder-tree [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; if there is a loop in
  the graph, the seq will get trapped in it.  (As the name suggests,
  this is an appropriate function if you know the graph is a tree.)"
  (->> (iterate step search)
       (take-while identity)
       (map #(second (last-edge %)))))
(defn preorder [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  Skips nodes that have appeared before."
  (preorder-tree (prune-seen search)))
