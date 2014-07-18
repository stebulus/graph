(ns gmwbot.graph-test
  (:use clojure.test gmwbot.graph))
(deftest vmap
  (is (= {0 [1 1 0] 1 [] 2 []}
         (vertex-map #(mod % 3) {0 [1 4 3] 2 []}))))
(deftest sccs
  (is (= (scc {:top [:left :right] :left [:top] :right []})
         #{#{:top :left} #{:right}}))
  (is (= (scc {:top [:left :right] :left [] :right [:top]})
         #{#{:top :right} #{:left}}))
  (is (= (scc {:top [:left :right] :left [:top] :right [:top]})
         #{#{:top :left :right}}))
  (is (= (scc {:top [:left :right] :left [:top] :right [:left]})
         #{#{:top :left :right}}))
  (is (= (scc {:top [:left :right] :left [] :right [:top :left]})
         #{#{:top :right} #{:left}}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (scc (merge-with into four {4 [1]}))
           #{#{1 2 3 4}}))
    (is (= (scc (merge-with into four {4 [2]}))
           #{#{1} #{2 3 4}}))
    (is (= (scc (merge-with into four {4 [3]}))
           #{#{1} #{2 3 4}}))))
(deftest condensation
  (is (= (condense {:top [:left :right] :left [:top] :right []})
         {#{:top :left} [#{:right}] #{:right} []}))
  (is (= (condense {:top [:left :right] :left [] :right [:top]})
         {#{:top :right} [#{:left}] #{:left} []}))
  (is (= (condense {:top [:left :right] :left [:top] :right [:top]})
         {#{:top :left :right} []}))
  (is (= (condense {:top [:left :right] :left [:top] :right [:left]})
         {#{:top :left :right} []}))
  (is (= (condense {:top [:left :right] :left [] :right [:top :left]})
         {#{:top :right} [#{:left}] #{:left} []}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (condense (merge-with into four {4 [1]}))
           {#{1 2 3 4} []}))
    (is (= (condense (merge-with into four {4 [2]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))
    (is (= (condense (merge-with into four {4 [3]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))))
