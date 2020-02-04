(ns com.github.hindol.euler
  (:require
   [clojure.core.match :refer [match]]
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as math]
   [clojure.set :as s]
   [clojure.string :as str]
   [clojure.tools.trace :refer [deftrace trace-forms]]
   [com.github.hindol.euler.roman :as roman])
  (:gen-class))

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

(defn factors-of
  [x]
  (let [prime-factors (prime-factorize x)]
    (map #(inc (reduce * %))
         (drop 1 (combo/subsets prime-factors)))))

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

(defn rifle-shuffle
  [deck]
  {:pre [(-> deck count even?)]}
  (let [half (quot (count deck) 2)]
    (apply mapcat vector (split-at half deck))))

(set! *warn-on-reflection* :warn-on-boxed)
(set! *unchecked-math* false)

(defn factorial-seq
  []
  (letfn [(step [[n f]]
            [(inc n) (*' f (inc n))])]
    (cons [0 1]
          (iterate step [1 1]))))

(defn power-seq
  ([x] (power-seq x 1))
  ([x begin]
   (map vector
        (iterate inc begin)
        (iterate #(* x %) (math/expt x begin))))
  ([x begin end]
   {:pre [(<= begin end)]}
   (take (- end begin) (power-seq x begin))))

(defn int-log
  "Returns how many times b can evenly divide x."
  [x b]
  (let [squares (reverse
                 (map vector
                      (iterate #(* 2 %) 1)
                      (take-while #(zero? (rem x %))
                                  (iterate #(*' % %) b))))]
    (loop [[[i m] [j n] & squares] squares]
      (if (nil? n)
        i
        (if (zero? (rem x (* m n)))
          (recur (cons [(+ i j) (* m n)] squares))
          (recur (cons [i m] squares)))))))

(defn prime?
  [x]
  (not (some #(zero? (rem x %))
             (take-while #(<= (* % %) x)
                         (iterate inc 2)))))

(defn kempner-seq
  [p]
  {:pre [prime? p]}
  (letfn [(step
           [[x k carry]]
           (if (pos? carry)
             [(* p x) k (dec carry)]
             [(* p x) (+ p k) (dec (int-log (+ p k) p))]))]
    (map pop (iterate step [p p 0]))))

(defn solve-119
  []
  (take 10 (kempner-seq 7)))

(defn -main
  [& _]
  (let [base   29
        raised (math/expt base 1000)]
    (dotimes [_ 1000]
      (int-log raised base))))

(time
 (-main))
