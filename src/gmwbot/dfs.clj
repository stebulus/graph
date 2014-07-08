(ns gmwbot.dfs)
(defprotocol DepthFirstSearch
  (down [this])
  (across [this])
  (up [this])
  (failed? [this])
  (last-edge [this]))
(defrecord StackDFS [failed stack children]
  DepthFirstSearch
  (down [this]
    (let [s (seq (children (first (peek stack))))]
      (if (nil? s)
        (StackDFS. true stack children)
        (StackDFS. false (conj stack s) children))))
  (across [this]
    (let [s (next (peek stack))]
      (if (nil? s)
        (StackDFS. true stack children)
        (StackDFS. false (conj (pop stack) s) children))))
  (up [this]
    (let [s (pop stack)]
      (if (empty? s)
        (StackDFS. true stack children)
        (StackDFS. false s children))))
  (failed? [this]
    (. this failed))
  (last-edge [this]
    [(first (peek (pop stack)))
     (first (peek stack))]))
(defn dfs [graph start]
  (StackDFS. false [[start]] graph))
