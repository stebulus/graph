(ns gmwbot.dfs
  (:refer-clojure :exclude [reduce])
  (:require [clojure.core :as core]))

(defprotocol DepthFirstSearch
  (down [this])
  (across [this])
  (up [this])
  (current [this])
  (reroot [this]))

(declare dfs)
(defrecord StackDFS [children stack]
  DepthFirstSearch
  (down [this]
    (some->> (seq (children (first (peek stack))))
             (conj stack)
             (StackDFS. children)))
  (across [this]
    (some->> (next (peek stack))
             (conj (pop stack))
             (StackDFS. children)))
  (up [this]
    (let [s (pop stack)]
      (when-not (empty? s)
        (StackDFS. children s))))
  (current [this]
    (first (peek stack)))
  (reroot [this]
    (dfs children (current this))))
(defn dfs [graph start]
  (StackDFS. graph [[start]]))

(defn skip [move pred search]
  (->> (iterate move search)
       (take-while some?)
       (drop-while pred)
       (first)))
(defn scan [move pred search]
  (skip move #(not (pred %)) search))

(defn loop? [search]
  (let [curr (current search)]
    (->> (iterate up search)
         (take-while some?)
         (drop 1)
         (map current)
         (some #(= curr %)))))

(defprotocol Wrapper
  (unwrap [this]))

(declare never-move never)
(defrecord NeverDFS [pred search]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (never-move down pred search))
  (across [this]
    (never-move across pred search))
  (up [this]
    (never-move up pred search))
  (current [this]
    (current search))
  (reroot [this]
    (never pred (reroot search))))
(defn- never-move [move pred search]
  (when-some [s (move search)]
    (assert (not (pred s)))
    (NeverDFS. pred s)))
(defn never [pred search]
  (never-move identity pred search))

(declare seen-move record-seen)
(defrecord SeenDFS [seen search]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (seen-move down seen search))
  (across [this]
    (seen-move across seen search))
  (up [this]
    (seen-move up seen search))
  (current [this]
    (current search))
  (reroot [this]
    (record-seen (reroot search))))
(defn- seen-move [move seen search]
  (some->> (move search)
           (SeenDFS. (conj seen (current search)))))
(defn record-seen [search]
  (SeenDFS. #{} search))
(defn seen? [seendfs]
  (contains? (.seen seendfs) (current seendfs)))

(declare prune)
(defrecord PrunedDFS [pred search]
  Wrapper
  (unwrap [this] search)
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
    (current search))
  (reroot [this]
    (prune pred (reroot search))))
(defn prune [pred search]
  (PrunedDFS. pred search))

(defn prune-seen [search]
  (prune seen? (record-seen search)))

(declare stepper-move stepper)
(defrecord StepperDFS [search inbound]
  Wrapper
  (unwrap [this] search)
  DepthFirstSearch
  (down [this]
    (stepper-move this down true))
  (across [this]
    (stepper-move this across true))
  (up [this]
    (stepper-move this up false))
  (current [this]
    (current search))
  (reroot [this]
    (stepper (reroot search))))
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

(defn reduce [f initf search]
  (core/reduce (fn [stack s]
                   (if (inbound? s)
                     (conj stack (initf s))
                     (let [top (peek stack)
                           stack (pop stack)]
                       (if (empty? stack)
                         top
                         (conj (pop stack)
                               (f (peek stack) top))))))
               []
               (->> (reroot search)
                    (never loop?)
                    (stepper)
                    (iterate step)
                    (take-while some?))))
