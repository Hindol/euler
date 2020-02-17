(ns com.github.hindol.euler.sieves
  (:require
   [criterium.core :as criterium]))

(set! *warn-on-reflection* :warn-on-boxed)

(defn primes [^long n]
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

(defn prime-factors
  [n]
  (let [cache (object-array (repeat (inc n) []))]
    (doseq [i      (range 2 (inc n))
            :when  (empty? (aget cache i)) ; no factors till now => prime!
            j      (iterate #(* i %) i)
            :while (<= j n)
            k      (range j (inc n) j)]
      (aset cache k (conj (aget cache k) i)))
    (map-indexed vector cache)))
