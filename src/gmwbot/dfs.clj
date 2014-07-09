(ns gmwbot.dfs)

(defprotocol DepthFirstSearch
  (down [this])
  (across [this])
  (up [this])
  (last-edge [this]))

(defrecord StackDFS [stack children]
  DepthFirstSearch
  (down [this]
    (when-some [s (seq (children (first (peek stack))))]
      (StackDFS. (conj stack s) children)))
  (across [this]
    (when-some [s (next (peek stack))]
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
  (->> (iterate across search)
       (take-while identity)
       (filter #(pred (second (last-edge %))))
       (first)))
(defn scan-children [search pred]
  (some-> (down search)
          (scan-across pred)))

(defrecord PrunedDFS [search seen]
  DepthFirstSearch
  (down [this]
    (when-some [s (scan-children search #(not (contains? seen %)))]
      (PrunedDFS. s (conj seen (second (last-edge s))))))
  (across [this]
    (when-some [s (scan-across search #(not (contains? seen %)))]
      (PrunedDFS. s (conj seen (second (last-edge s))))))
  (up [this]
    (when-some [s (up search)]
      (PrunedDFS. s seen)))
  (last-edge [this]
    (last-edge search)))
(defn prune-seen [search]
  (PrunedDFS. search #{(second (last-edge search))}))

(defn step [search]
  (or (down search)
      (->> (iterate up search)
           (take-while identity)
           (map across)
           (some identity))))
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
