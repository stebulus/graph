(ns gmwbot.dfs)
(defprotocol DepthFirstSearch
  (down [this])
  (across [this])
  (up [this])
  (fail [this])
  (success? [this])
  (last-edge [this]))
(defrecord StackDFS [success stack children]
  DepthFirstSearch
  (down [this]
    (let [s (seq (children (first (peek stack))))]
      (if (nil? s)
        (StackDFS. false stack children)
        (StackDFS. true (conj stack s) children))))
  (across [this]
    (let [s (next (peek stack))]
      (if (nil? s)
        (StackDFS. false stack children)
        (StackDFS. true (conj (pop stack) s) children))))
  (up [this]
    (let [s (pop stack)]
      (if (empty? s)
        (StackDFS. false stack children)
        (StackDFS. true s children))))
  (fail [this]
    (StackDFS. false stack children))
  (success? [this]
    success)
  (last-edge [this]
    [(first (peek (pop stack)))
     (first (peek stack))]))
(defn dfs [graph start]
  (StackDFS. true [[start]] graph))
(defn scan-across [search pred]
  (if (pred (second (last-edge search)))
    search
    (let [s (across search)]
      (if (success? s)
        (recur s pred)
        s))))
(defn scan-children [search pred]
  (let [s (down search)]
    (if (success? s)
      (let [s (scan-across s pred)]
        (if (success? s)
          s
          (fail (up s))))
      s)))
(defrecord PrunedDFS [success search seen]
  DepthFirstSearch
  (down [this]
    (let [s (scan-children search #(not (contains? seen %)))]
      (if (success? s)
        (PrunedDFS. true s (conj seen (second (last-edge s))))
        (PrunedDFS. false s seen))))
  (across [this]
    (let [s (scan-across search #(not (contains? seen %)))]
      (if (success? s)
        (PrunedDFS. true s (conj seen (second (last-edge s))))
        (PrunedDFS. false search seen))))
  (up [this]
    (let [s (up search)]
      (PrunedDFS. (success? s) s seen)))
  (fail [this]
    (PrunedDFS. false search seen))
  (success? [this]
    success)
  (last-edge [this]
    (last-edge search)))
(defn prune-seen [search]
  (PrunedDFS. true search #{(second (last-edge search))}))

(defn step [search]
  (let [s (down search)]
    (if (success? s)
      s
      (loop [s s]
        (let [s (across s)]
          (if (success? s)
            s
            (let [s (up s)]
              (if (success? s)
                (recur s)
                s))))))))

(defn preorder-tree [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  If a node is reachable in more than one way from the initial node,
  it will appear in the seq multiple times; if there is a loop in
  the graph, the seq will get trapped in it.  (As the name suggests,
  this is an appropriate function if you know the graph is a tree.)"
  (->> (iterate step search)
       (take-while success?)
       (map #(second (last-edge %)))))
(defn preorder [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  Skips nodes that have appeared before."
  (preorder-tree (prune-seen search)))
