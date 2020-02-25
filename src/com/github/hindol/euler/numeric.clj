(ns com.github.hindol.euler.numeric
  (:require
   [clojure.math.combinatorics :as combo]))

(defn factorial
  "Returns the factorial of n."
  [n & {:keys [reduce-fn]
        :or   {reduce-fn *'}}]
  {:pre [(not (neg? n))]}
  (if (zero? n)
    1
    (reduce reduce-fn (range 2 (inc n)))))

(defn prime?
  [x]
  (not (some #(zero? (rem x %))
             (take-while #(<= (* % %) x)
                         (iterate inc 2)))))

(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from (fn primes-from
                        [n [f & r]]
                        (if (some #(zero? (rem n %))
                                  (take-while #(<= (* % %) n) primes))
                          (recur (+ n f) r)
                          (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel       (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                              6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                              2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn coprime-pairs
  []
  (lazy-seq
   (letfn [(next-pairs
             [[x y]]
             [[(- (+ x x) y) x]
              [(+ x x y) x]
              [(+ x y y) y]])
           (pairs-from
             [seed]
             (let [pair     (first seed)
                   children (next-pairs pair)]
               (lazy-seq
                (cons pair
                      (pairs-from
                       (into (disj seed pair) children))))))]
     (pairs-from (sorted-set [2 1] [3 1])))))

(defn prime-factorize
  [x]
  (loop [x  (long x)
         ps primes
         fs (list)]
    (let [p (long (first ps))]
      (if (> (* p p) x)
        (if (< 1 x)
          (cons x fs)
          (into [] (reverse fs)))
        (if (zero? (rem x p))
          (recur (quot x p) ps (cons p fs))
          (recur x (rest ps) fs))))))

(defn factors-of
  [x]
  (let [prime-factors (prime-factorize x)]
    (map #(inc (reduce * %))
         (drop 1 (combo/subsets prime-factors)))))

(defn int-log
  "Returns how many times b can evenly divide x."
  [x b]
  (let [square    #(*' % %)
        exponents (iterate #(* 2 %) 1)
        powers    (take-while #(zero? (rem x %))
                              (iterate square b))]
    (if (empty? powers)
      0
      (loop [[[i m] [j n] & r] (reverse (map vector exponents powers))]
        (if (nil? n)
          i
          (if (zero? (rem x (* m n)))
            (recur (cons [(+ i j) (* m n)] r))
            (recur (cons [i m] r))))))))
