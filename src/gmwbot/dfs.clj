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

(defn skip [move pred search]
  (->> (iterate move search)
       (take-while some?)
       (drop-while pred)
       (first)))
(defn scan [move pred search]
  (skip move #(not (pred %)) search))

(declare seen-move)
(defrecord SeenDFS [seen search]
  DepthFirstSearch
  (down [this]
    (seen-move down seen search))
  (across [this]
    (seen-move across seen search))
  (up [this]
    (seen-move up seen search))
  (current [this]
    (current search)))
(defn- seen-move [move seen search]
  (some->> (move search)
           (SeenDFS. (conj seen (current search)))))
(defn record-seen [search]
  (SeenDFS. #{} search))
(defn seen? [seendfs]
  (contains? (.seen seendfs) (current seendfs)))

(defrecord PrunedDFS [pred search]
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
    (current search)))
(defn prune [pred search]
  (PrunedDFS. pred search))

(defn prune-seen [search]
  (prune seen? (record-seen search)))

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

(declare stepper-move)
(defrecord StepperDFS [search inbound]
  DepthFirstSearch
  (down [this]
    (stepper-move this down true))
  (across [this]
    (stepper-move this across true))
  (up [this]
    (stepper-move this up false))
  (current [this]
    (current search)))
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
