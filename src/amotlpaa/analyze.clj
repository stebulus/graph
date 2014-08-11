(ns amotlpaa.analyze
  (:use clojure.set)
  (:require [amotlpaa.df :as df]
            [amotlpaa.graph :as graph]))

;; LL(1) parsing

(defn make-nullable?
  "Returns a function which takes one or more symbols as arguments
  and returns whether that sequence of symbols is nullable in the
  given grammar.  The returned function is memoized."
  [productions]
  ;; We use df/memo-reduce with a virtual graph where the symbols
  ;; (terminal and nonterminal) of the grammar appear as nodes
  ;; [:single symbol] and the right-hand sides of productions
  ;; appear as nodes [:list symbols].  The children of a symbol
  ;; node are the nodes representing its right-hand sides; the
  ;; children of a right-hand-side node are the nodes representing
  ;; the symbols in the expansion.  Nullability is then a matter of
  ;; evaluation: a symbol is nullable if any of its children are,
  ;; and a right-hand side is nullable if all of its children are.
  ;; (Nonterminal symbols have no children, so they are not nullable;
  ;; empty right-hand sides have no children, so they *are* nullable.)
  (let [memo (atom {})
        ensure (fn [memo target]
                 (df/memo-reduce memo
                                 (fn [state x]
                                   (if (= state x)
                                     state
                                     (reduced x)))
                                 (fn [[tag _]] (= :list tag))
                                 (df/dfc (fn [[tag x]]
                                           (case tag
                                             :single (map #(list :list %) (productions x))
                                             :list (map #(list :single %) x)
                                             (assert false)))
                                         target)))]
    (fn [& symbols]
      (let [k [:list symbols]]
        (get (swap! memo ensure k) k)))))
(defn nullables
  "The nullable nonterminal symbols of the given grammar, which is
  a map nonterminal -> list of expansions, where an expansion is a
  (possibly empty) list of nonterminals and terminals."
  [productions]
  (let [nullable? (make-nullable? productions)]
    (filter nullable? (keys productions))))

(defn- take-until [pred xs]
  (lazy-seq
    (when (seq xs)
      (let [x (first xs)]
        (if (pred x)
          [x]
          (cons x (take-until pred (rest xs))))))))
(defn make-first-set
  [productions nullable?]
  (let [memo (atom {})
        ensure (fn [memo target]
                 (df/memo-reduce memo
                                 union
                                 (fn [[tag x]]
                                   (if (and (= tag :single)
                                            (nil? (productions x)))
                                     #{x}
                                     #{}))
                                 (df/dfc
                                   (fn [[tag x]]
                                     (if (= tag :single)
                                       (map #(list :list %) (productions x))
                                       (map #(list :single %) (take-until #(not (nullable? %)) x))))
                                   target)))]
    (fn [& symbols]
      (let [k [:list symbols]]
        (get (swap! memo ensure k) k)))))
(defn first-sets
  ([productions]
    (first-sets productions (make-nullable? productions)))
  ([productions nullable?]
    (->> (keys productions)
         (map (juxt identity (make-first-set productions nullable?)))
         (into {}))))

(defn follow-deps
  ([productions]
    (let [nullable? (make-nullable? productions)]
      (follow-deps productions
                   nullable?
                   (make-first-set productions nullable?))))
  ([productions nullable? first-set]
    (graph/into (graph/empty (map first productions))
                (for [[lhs rhses] productions
                      rhs rhses
                      sym (take-until #(not (nullable? %))
                                      (reverse rhs))
                      :when (contains? productions sym)]
                  [sym lhs]))))
(defn- base-follow [rhs nullable? first-set]
  (let [revrhs (reverse rhs)]
    (map (fn [a b] [a b])
         revrhs
         (reductions (fn [fol sym]
                       (let [fir (first-set sym)]
                         (if (nullable? sym)
                           (union fol fir)
                           fir)))
                     #{}
                     revrhs))))
(defn follow-sets
  [productions nullable? first-set]
  (let [deps (follow-deps productions nullable? first-set)
        sccm (graph/scc-map deps)
        cdeps (graph/simplify (graph/map sccm deps))
        base (reduce (fn [m [k v]]
                       (merge-with union m {k v}))
                     {}
                     (->> (for [rhses (vals productions) rhs rhses] rhs)
                          (mapcat #(base-follow % nullable? first-set))
                          (filter (fn [[k v]] (contains? productions k)))
                          (map (fn [[k v]] [(sccm k) v]))))]
    (into {}
          (for [[component folset]
                 (reduce #(df/memo-reduce
                            %1
                            union
                            base
                            (df/dfc cdeps %2))
                         {}
                         (graph/vertices-with-duplicates cdeps))
                v component]
            [v folset]))))

(defn parse-table [productions]
  (let [nullable? (make-nullable? productions)
        first-set (make-first-set productions nullable?)
        follow-set (follow-sets productions nullable? first-set)]
    (reduce (fn [m [k rhs]]
              (if-some [existing-revrhs (get-in m k)]
                (throw (IllegalArgumentException.
                         (print-str "parse conflict:"
                                    k "has expansions"
                                    (reverse existing-revrhs)
                                    "and" rhs)))
                (assoc-in m k (reverse rhs))))
            {}
            (->> (for [[sym rhses] productions
                       rhs rhses]
                   [sym rhs])
                 (mapcat (fn [[sym rhs]]
                           (concat
                             (for [x (apply first-set rhs)]
                               [[sym x] rhs])
                             (if (apply nullable? rhs)
                               (for [x (conj (follow-set sym) nil)]
                                 [[sym x] rhs])
                               []))))))))
(defn consume-token [table token stack]
  (if (empty? stack)
    (if (nil? token)
      nil
      (throw (IllegalStateException.
               (print-str "expected EOF, not" token))))
    (let [sym (peek stack)]
      (if (contains? table sym)
        (if-some [revrhs (get-in table [sym token])]
          (recur table
                 token
                 (reduce conj (pop stack) revrhs))
          (throw (IllegalStateException.
                   (print-str "unexpected" token
                              "when expanding" sym))))
        (if (= sym token)
          (pop stack)
          (throw (IllegalStateException.
                   (print-str "unexpected" token
                              "when" sym "expected"))))))))
