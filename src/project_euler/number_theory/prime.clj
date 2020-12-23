(ns project-euler.number-theory.prime
  (:require [criterium.core :refer [bench]]))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (* b (/ a (gcd a b))))

(defn extended-gcd
  "Calculates the extended GCD and returns a vector [r x y]. r is the GCD x is
  the multiplicative inverse of a modulo b, and y is the multiplicative inverse
  of b modulo a."
  [a b]
  (if (zero? b)
    [a [1 0]]
    (let [q (quot a b)
          r (mod a b)
          [gcd [x y]] (extended-gcd b r)]
      [gcd [y (- x (* q y))]])))

(defn factor? [num den]
  (zero? (rem num den)))

(defn lazy-primes []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                    (lazy-seq (next-primes (next-sieve sieve candidate)
                                           (+ candidate 2))))))]
    (cons 2N (lazy-seq (next-primes {} 3N)))))

;; p prime then p mod 6 = 1 or 5
(defn- prime?* [num]
  (loop [iter 5
         top  (Math/sqrt num)]
    (cond
      (> iter top)
      true

      (or (zero? (mod num iter))
          (zero? (mod num (+ 2 iter))))
      false

      :else (recur (+ 6 iter) top))))

(defn prime? [num]
  (cond
    (<= num 3) (< 1 num)
    (or (zero? (mod num 2))
        (zero? (mod num 3))) false
    :else (prime?* num)))