(ns project-euler.number-theory.combinatorics)

(defn ! [num]
  (assert (integer? num))
  (reduce * (range 1N (inc num))))

(defn choose
  "Calculates n choose r."
  [n r]
  (if (<= 0 r n)
    (let [r (min r (- n r))]
      (loop [res 1N
             i   1N
             j   n]
        (if (<= i r)
          (recur (/ (* res j) i) (inc i) (dec j))
          res)))
    0))