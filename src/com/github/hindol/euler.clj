(ns com.github.hindol.euler
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as math]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as s]
   [clojure.string :as str]
   [clojure.tools.trace :refer [deftrace trace-ns untrace-ns]]
   [com.github.hindol.euler.roman :as roman])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(map (partial apply min) [[2 4 3 5 1] [2.0 4.0 3.0 5.0 1.0]])

(defn pentagonal-seq
  []
  (let [xf (comp
            (mapcat (juxt identity -))
            (map #(/ (* % (dec (* 3 %))) 2)))] ;; k (3k - 1) / 2
    (sequence xf (iterate inc 1))))

(def cached-pentagonal-seq
  (pentagonal-seq))

;; https://projecteuler.net/problem=78
;;
;; *
;;
;; **
;; * *
;;
;; ***
;; ** *
;; * * *
;;
;; ****
;; *** *
;; ** **
;; ** * *
(def count-partitions
  "Given n coins, how many distinct partitions are possible?"
  (memoize
   (fn
     [n]
     {:pre  [(not (neg? n))]
      :post [(not (neg? %))]}
     (cond
       (neg? n)  0
       (zero? n) 1
       :else     (let [xf    (comp
                              (take-while #(<= % n))
                              (map #(count-partitions (- n %))))
                       terms (sequence xf (pentagonal-seq))
                       signs (cycle [+1 +1 -1 -1])]
                   (reduce +' (map * signs terms)))))))

(defn solve-78
  []
  (let [n  6
        xf (comp
            (map count-partitions)
            (take-while #(pos? (rem % 1000000))))]
    (+ n (count (sequence xf (iterate inc n))))))

(defn factorial
  "Returns the factorial of n."
  [n & {:keys [reduce-fn]
        :or   {reduce-fn *'}}]
  {:pre [(not (neg? n))]}
  (if (zero? n)
    1
    (reduce reduce-fn (range 2 (inc n)))))

(defn max-by
  [key-fn & args]
  (last (sort-by key-fn args)))

(defn ordered?
  [xs]
  (or (empty? xs) (apply <= xs)))

(defn gen-targets
  [s]
  {:pre [(and (sorted? s) (set? s))]}
  (if (= 1 (count s))
    s
    (apply s/union
           (for [x    s
                 y    (gen-targets (disj s x))
                 :let [targets (sorted-set (+ x y)
                                           (- x y)
                                           (- y x)
                                           (* x y))]]
             (cond-> targets
               (not (zero? y)) (conj (/ x y))
               (not (zero? x)) (conj (/ y x)))))))

(defn count-consecutive
  [coll]
  (->> coll
       (partition 2 1)
       (map #(apply - %))
       (take-while #{-1})
       count
       inc))

(defn solve-93
  []
  (apply max-by second
         (for [p (combo/combinations (range 1 10) 4)]
           [p (->> (gen-targets (into (sorted-set) p))
                   (filter #(and (pos? %) (integer? %)))
                   (count-consecutive))])))

(defn prob-half-seq
  []
  (let [step (fn [[blue total]]
               [(+ (* 3 blue) (* 2 total) (- 2))
                (+ (* 4 blue) (* 3 total) (- 3))])]
    (iterate step [15 21])))

(defn solve-100
  []
  (first
   (drop-while #(<= (second %) 1000000000000)
               (prob-half-seq))))

(defn reverse-digits
  [n]
  (let [n (int n)]
    (loop [fwd  (quot n 10)
           bkwd (rem n 10)]
      (if (zero? fwd)
        bkwd
        (recur (quot fwd 10) (+ (* 10 bkwd) (rem fwd 10)))))))

(defn digit
  [ch]
  (Character/digit ^char ch 10))

(defn reversible-number?
  [n]
  (let [n (int n)
        s (str n)
        f (first s)
        l (last s)]
    (cond
      (= \0 l)        false
      (even?
       (+ (int (digit f))
          (int (digit l)))) false
      :else           (every? #{\1 \3 \5 \7 \9}
                              (str
                               (+ (int n) (int (reverse-digits n))))))))

(defn solve-145
  []
  (count
   (filter reversible-number? (range 100000000))))

(defn sieve [^long n]
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
                  (do
                    (aset primes i false)
                    (recur (+ i p))))))
            (recur (+ p 2))))))))

(defn remainder-seq
  []
  (let [cached-sieve (sieve 1000000000)
        remainder    (fn remainder
                       [^long n]
                       (let [nth-prime (int (nth cached-sieve (dec n)))]
                         (int (rem (+ ^long (math/expt (dec nth-prime) n)
                                      ^long (math/expt (inc nth-prime) n))
                                   (* nth-prime nth-prime)))))]
    (map (juxt identity remainder) (iterate (partial + 2) 7037))))

(defn solve-123
  []
  (first
   (filter #(> (second %) 10000000000) (remainder-seq))))

(defn solve-89
  []
  (let [roman-numerals (-> "resources/p089_roman.txt"
                           slurp
                           (str/split #"\n"))
        before         (->> roman-numerals
                            (map count)
                            (reduce +))
        after          (->> roman-numerals
                            (map roman/minimal-roman)
                            (map count)
                            (reduce +))]
    (- before after)))

(defn read-clockwise
  [v]
  (for [side [[0 1 2] [3 2 4] [5 4 6] [7 6 8] [9 8 1]]]
    (map #(nth v %) side)))

(defn magic-5-gon?
  [v]
  (apply = (map #(reduce + %) (read-clockwise v))))

(defn solve-68
  []
  (let [xf (comp
            (filter magic-5-gon?)
            (map read-clockwise)
            (filter #(= (ffirst %)
                        (->> %
                             (map first)
                             (apply min))))
            (map flatten)
            (map #(apply str %))
            (filter #(= 16 (count %))))]
    (->> [1 2 3 4 5 6 7 8 9 10]
         combo/permutations
         (sequence xf))))

(defn merge-ordered
  "Merge one or more ordered sequences into one ordered sequence."
  ([x] x)
  ([x y]
   (lazy-seq
    (cond
      (and (seq x)
           (seq y)) (if (<= ^long (first x) ^long (first y))
                      (cons (first x) (merge-ordered (rest x) y))
                      (cons (first y) (merge-ordered x (rest y))))
      (seq x) x
      (seq y) y)))
  ([x y & colls]
   (loop [combined  (merge-ordered x y)
          remaining colls]
     (if (empty? remaining)
       combined
       (recur
        (merge-ordered combined (first remaining))
        (rest remaining))))))

(defn ^:private next-coprime-pairs
  [[^long x ^long y]]
  [[(- (+ x x) y) x]
   [(+ x x y) x]
   [(+ x y y) y]])

(defn coprime-pair-seq
  ([]
   (coprime-pair-seq (sorted-set [2 1] [3 1])))
  ([seed]
   (lazy-seq
    (let [pair     (first seed)
          children (next-coprime-pairs pair)]
      (cons pair
            (coprime-pair-seq
             (into (disj seed pair) children)))))))

(def prime-seq
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from (fn primes-from
                        [n [f & r]]
                        (if (some #(zero? (rem n %))
                                  (take-while #(<= (* % %) n) prime-seq))
                          (recur (+ n f) r)
                          (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel       (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                              6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                              2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn prime-factorize
  [x]
  (loop [x  x
         ps prime-seq
         fs []]
    (let [p (first ps)]
      (if (> (* p p) x)
        (if (< 1 x)
          (conj fs x)
          fs)
        (if (zero? (rem x p))
          (recur (quot x p) ps (conj fs p))
          (recur x (rest ps) fs))))))

(defn totient
  [x]
  (long
   (reduce *
           (cons x
                 (for [p (dedupe (prime-factorize x))]
                   (- 1 (/ 1 p)))))))

(defn resilience
  [x]
  (/ (totient x) (dec x)))

(defn digits
  [^long x]
  (map #(Character/digit ^char % 10) (str x)))

(defn digital-sum
  [x]
  (reduce + (digits x)))

(defn fibonacci-seq
  []
  (map first
       (letfn [(step [[^long x ^long y]] [y (+ x y)])]
         (iterate step [0 1]))))

(def cached-fibonacci-seq
  (fibonacci-seq))

(defn s
  [^long x]
  (dec
   (long (reduce * (inc (rem x 9)) (repeat (quot x 9) 10)))))

(defn rev-bits
  "The bits of integer x, in reverse order."
  [^long x]
  (let [mask 2r1]
    (loop [x    x
           bits []]
      (if (zero? x)
        bits
        (recur
         (bit-shift-right x 1)
         (conj bits (bit-and x mask)))))))

(defn expt
  ^long [^long x ^long p]
  {:post [(if (or (pos? x) (even? p))
            (pos? %)
            (neg? %))]}
  (letfn [(mulmod
            [x y]
            (rem (* x y) 1000000007))]
    (if (zero? p)
      1
      (reduce mulmod
              (remove #{0}
                      (map *
                           (iterate #(mulmod % %) x)
                           (rev-bits p)))))))

(defn S
  [^long x]
  {:post [(<= 0 % 1000000006)]}
  (let [mulmod (fn
                 [x y]
                 (rem (* x y) 1000000007))
        n      (quot x 9)
        r      (+ 2 (rem x 9))]
    (rem
     (/ (- (mulmod (expt 10 n)
                   (+ 10 (mulmod r (dec r))))
           (mulmod 2
                   (+ r (mulmod 9 n) 4)))
        2)
     1000000007)))

(defn -main
  [& _]
  (let [addmod  (fn
                  [x y]
                  (rem (+ x y) 1000000007))
        cnt     89
        indices (take (inc cnt)
                      (drop 2 (fibonacci-seq)))]
    (map S indices)))

(time (-main))
