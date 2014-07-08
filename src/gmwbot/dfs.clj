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
    (. this success))
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
