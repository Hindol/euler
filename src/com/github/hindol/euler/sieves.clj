(ns com.github.hindol.euler.sieves
  (:require
   [criterium.core :as criterium]))

(set! *warn-on-reflection* :warn-on-boxed)

(defn primes-till [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (filter #(aget primes %)
                          (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p)))))
            (recur (+ p 2))))))))

(defn prime-factorize-till
  [n]
  (let [smallest-factors (int-array (range (inc n)))
        wheel            (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                                 6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                                 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])
        xf               (comp
                          (take-while #(<= % n))
                          (filter #(= % (aget smallest-factors %))))
        factors-of       (fn factors-of
                           [x]
                           (loop [x  (long x)
                                  fs []]
                             (let [y (aget smallest-factors x)]
                               (if (= x y)
                                 (conj fs y)
                                 (recur (quot x y) (conj fs y))))))]
    (doseq [i     (concat
                   [2 3 5 7]
                   (sequence xf (reductions + 11 wheel)))
            j     (range (+ i i) (inc n) i)
            :when (= j (aget smallest-factors j))]
      (aset-int smallest-factors j i))
    (let [factors (object-array (inc n))]
      (aset factors 0 [])
      (aset factors 1 [])
      (loop [i 2]
        (if (> i n)
          (vec factors)
          (do
            (aset factors i (factors-of i))
            (recur (inc i))))))))

(time
 (count
  (prime-factorize-till 100000000)))
