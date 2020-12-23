(ns project-euler.euler152
  (:require [clojure.math.combinatorics :as combi]))

(def target (/ 1N 2N))

(def nums (map #(/ 1 (* % %)) (range 2N 81N)))

(defn success [coll]
  (= target (apply + coll)))

(defn euler152 []
  (count (combi/permutations nums)))