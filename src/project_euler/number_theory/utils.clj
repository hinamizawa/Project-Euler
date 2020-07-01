(ns project-euler.number-theory.utils)

(defn int-sqrt [num]
  (bigint (.sqrt ^BigInteger (biginteger num))))

(defn perfect-square? [num]
  (let [sqrt (int-sqrt num)]
    (= num (* sqrt sqrt))))