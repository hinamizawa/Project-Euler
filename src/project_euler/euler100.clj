(ns project-euler.euler100
  (:require [project-euler.number-theory.utils :refer [perfect-square? int-sqrt]]
            [criterium.core :refer [quick-bench]])
  (:import [java.util LinkedList]))

;; prob = p(p-1)/n(n-1)
;; 2p(p-1) = n(n-1) gives
;; p = (1 \pm sqrt(2n^2-2n+1))/2

(def ^:const input 1000000000000N)
(def ^:const sqrt2 (Math/sqrt 2))

(defn- adjacent-prod [n]
  (* n (dec n)))

(defn euler100 [n]
  (loop [n n]
    (let [delta (inc (* 2 (adjacent-prod n)))
          sd    (int-sqrt delta)]
      (if (= delta (* sd sd))
        [n (/ (inc sd) 2)]
        (recur (if (zero? (mod n 4))
                 (inc n)
                 (+ n 3)))))))

(defn euler100-another [n]
  (loop [n n]
    (let [p (inc (bigint (/ n sqrt2)))]
      (if (= (* 2 (adjacent-prod p))
             (adjacent-prod n))
        [n p]
        (recur (if (zero? (mod n 4))
                 (inc n)
                 (+ n 3N)))))))

(defn -main []
  (println ()))
