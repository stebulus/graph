(ns gmwbot.graph-test
  (:use clojure.test)
  (:require [gmwbot.graph :as graph]))
(deftest vertex-map
  (is (= {0 [1 1 0] 1 [] 2 []}
         (graph/vertex-map #(mod % 3) {0 [1 4 3] 2 []}))))
(deftest scc
  (is (= (graph/scc {:top [:left :right] :left [:top] :right []})
         #{#{:top :left} #{:right}}))
  (is (= (graph/scc {:top [:left :right] :left [] :right [:top]})
         #{#{:top :right} #{:left}}))
  (is (= (graph/scc {:top [:left :right] :left [:top] :right [:top]})
         #{#{:top :left :right}}))
  (is (= (graph/scc {:top [:left :right] :left [:top] :right [:left]})
         #{#{:top :left :right}}))
  (is (= (graph/scc {:top [:left :right] :left [] :right [:top :left]})
         #{#{:top :right} #{:left}}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (graph/scc (merge-with into four {4 [1]}))
           #{#{1 2 3 4}}))
    (is (= (graph/scc (merge-with into four {4 [2]}))
           #{#{1} #{2 3 4}}))
    (is (= (graph/scc (merge-with into four {4 [3]}))
           #{#{1} #{2 3 4}}))))
(deftest condense
  (is (= (graph/condense {:top [:left :right] :left [:top] :right []})
         {#{:top :left} [#{:right}] #{:right} []}))
  (is (= (graph/condense {:top [:left :right] :left [] :right [:top]})
         {#{:top :right} [#{:left}] #{:left} []}))
  (is (= (graph/condense {:top [:left :right] :left [:top] :right [:top]})
         {#{:top :left :right} []}))
  (is (= (graph/condense {:top [:left :right] :left [:top] :right [:left]})
         {#{:top :left :right} []}))
  (is (= (graph/condense {:top [:left :right] :left [] :right [:top :left]})
         {#{:top :right} [#{:left}] #{:left} []}))
  (let [four {1 [2] 2 [3] 3 [4 2]}]
    (is (= (graph/condense (merge-with into four {4 [1]}))
           {#{1 2 3 4} []}))
    (is (= (graph/condense (merge-with into four {4 [2]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))
    (is (= (graph/condense (merge-with into four {4 [3]}))
           {#{1} [#{2 3 4}] #{2 3 4} []}))))