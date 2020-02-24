(ns com.github.hindol.euler.sieves)

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
  (let [cache (int-array (range (inc n)))
        n     (int n)]
    ;; Even numbers
    (loop [i (int 2)]
      (when (<= i n)
        (aset-int cache i 2)
        (recur (+ i 2))))
    ;; Odd numbers
    (loop [i (int 3)]
      (when (<= (* 3 i) n)
        (when (= i (aget cache i))
          (loop [j (* 3 i)]
            (when (<= j n)
              (when (= j (aget cache j))
                (aset-int cache j i))
              (recur (+ i i j)))))
        (recur (+ i 2))))
    (concat [[0 ()] [1 ()]]
            (map (fn [n]
                   (loop [x  n
                          fx ()]
                     (let [y (int (aget cache x))]
                       (if (= x y)
                         [n (conj fx y)]
                         (recur (quot x y) (cons y fx))))))
                 (range 2 (inc n))))))
