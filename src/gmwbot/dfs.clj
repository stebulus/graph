(ns gmwbot.dfs)

(defprotocol DepthFirstSearch
  (down [this])
  (across [this])
  (up [this])
  (current [this]))

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
  (current [this]
    (first (peek stack))))
(defn dfs [graph start]
  (StackDFS. [[start]] graph))

(defn scan-across [search pred]
  (->> (iterate across search)
       (take-while identity)
       (filter #(pred (current %)))
       (first)))
(defn scan-children [search pred]
  (some-> (down search)
          (scan-across pred)))

(declare pruned-move)
(defrecord PrunedDFS [search seen]
  DepthFirstSearch
  (down [this]
    (pruned-move this scan-children))
  (across [this]
    (pruned-move this scan-across))
  (up [this]
    (when-some [s (up search)]
      (PrunedDFS. s seen)))
  (current [this]
    (current search)))
(defn- pruned-move [pdfs scan]
  (when-some [s (scan (.search pdfs)
                      #(not (contains? (.seen pdfs) %)))]
    (PrunedDFS. s (conj (.seen pdfs) (current s)))))
(defn prune-seen [search]
  (PrunedDFS. search #{(current search)}))

(declare fail-on-loop-move)
(defrecord FailOnLoopDFS [search ancestors]
  DepthFirstSearch
  (down [this]
    (fail-on-loop-move this down ancestors))
  (across [this]
    (fail-on-loop-move this across (disj ancestors (current search))))
  (up [this]
    (when-some [s (up search)]
      (FailOnLoopDFS. s (disj ancestors (current search)))))
  (current [this]
    (current search)))
(defn- fail-on-loop-move [foldfs move ancestors]
  (when-some [s (move (.search foldfs))]
    (let [curr (current s)]
      (if (contains? ancestors curr)
        (throw (IllegalStateException. (str "loop: " s)))
        (FailOnLoopDFS. s (conj ancestors curr))))))
(defn fail-on-loop [search]
  (FailOnLoopDFS. search #{(current search)}))

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
       (map current)))
(defn preorder [search]
  "Returns a lazy seq of nodes as by a preorder traversal of search.
  Skips nodes that have appeared before."
  (preorder-tree (prune-seen search)))
