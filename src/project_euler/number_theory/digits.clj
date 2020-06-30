(ns project-euler.number-theory.digits)

(defn num->digits
  "Converts the number n into a list of digits in radix r. r defaults to 10."
  ([n]
   (map #(Character/getNumericValue ^Character %) (str n)))
  ([n r]
   (loop [n   n
          acc (transient [])]
     (if (zero? n)
       (or (rseq (persistent! acc))
           '(0))
       (recur (quot n r) (conj! acc (rem n r)))))))

(defn digital-sum
  "Returns the digital sum of the integer in radix r. r defaults to 10."
  ([n]
   (digital-sum n 10))
  ([n r]
   (loop [n n
          acc 0]
     (if (zero? n)
       acc
       (recur (quot n r)
              (+ acc (rem n r)))))))