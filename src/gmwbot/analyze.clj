(ns gmwbot.analyze)

;; Directed graphs with thresholds

(defn- decrement-thresholds
  "Returns [new-tgraph zeros], where new-tgraph is tgraph with the
  thresholds of nodes decremented, and zeros is a sequence of the
  nodes whose thresholds became zero."
  ([tgraph nodes]
    (decrement-thresholds tgraph nodes []))
  ([tgraph nodes zeros]
    (if-let [node (first nodes)]
      (if-let [[threshold children] (get tgraph node)]
        (let [newthresh (dec threshold)
              newgraph (assoc tgraph node [newthresh children])]
          (recur newgraph
                 (rest nodes)
                 (if (= 0 newthresh)
                   (conj zeros node)
                   zeros)))
        (recur tgraph (rest nodes) zeros))
      [tgraph zeros])))
(defn- reachable-queue
  [tgraph q]
  (lazy-seq
    (when-let [node (peek q)]
      (if-let [[threshold children] (get tgraph node)]
        (cons node
              (let [[new-tgraph zeros]
                     (decrement-thresholds (dissoc tgraph node) children)]
                (reachable-queue new-tgraph (into (pop q) zeros))))
        (reachable-queue tgraph (pop q))))))
(defn reachable
  "Returns a sequence of the nodes in tgraph which are reachable from
  nodes (including those nodes themselves).  The graph is a map key ->
  [threshold children], where threshold is an nonnegative integer
  and children is a sequence of keys, one for each outbound edge.
  A node with threshold t is reachable if it has at least t inbound
  edges from reachable nodes."
  [tgraph nodes]
  (reachable-queue tgraph (into clojure.lang.PersistentQueue/EMPTY nodes)))

;; LL(1) parsing

(defn- nullable-tgraph [productions]
  (merge-with
    (fn [[threshold edges] [_ new-edges]]
      [threshold (into edges new-edges)])
    (zipmap (for [k (keys productions)] [:lhs k])
            (repeat [1 []]))
    (into {} (for [[k rhses] (seq productions)
                   [i rhs] (map list (iterate inc 0) (seq rhses))]
               [[:rhs k i] [(count rhs) [[:lhs k]]]]))
    (into {} (for [[k rhses] (seq productions)
                   [i rhs] (map list (iterate inc 0) (seq rhses))
                   x (seq rhs)
                   :when (contains? productions x)]
               [[:lhs x] [nil [[:rhs k i]]]]))))
(defn nullables
  "The nullable nonterminal symbols of the given grammar, which is
  a map nonterminal -> list of expansions, where an expansion is a
  (possibly empty) list of nonterminals and terminals."
  [productions]
  (let [tgraph (nullable-tgraph productions)]
    (->> (reachable tgraph
                    (for [[k [threshold _]] (seq tgraph)
                          :when (= 0 threshold)]
                      k))
         (filter #(= :lhs (first %)))
         (map second))))
