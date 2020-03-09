(ns com.github.hindol.euler
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.math.numeric-tower :as math]
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.set :as s]
   [clojure.string :as str]
   [clojure.tools.trace :as trace]
   [com.github.hindol.euler.collections :as coll]
   [com.github.hindol.euler.modular :as modular]
   [com.github.hindol.euler.numeric :as numeric]
   [com.github.hindol.euler.roman :as roman]
   [com.github.hindol.euler.sieves :as sieves]
   [criterium.core :as criterium])
  (:gen-class))

(set! *warn-on-reflection* :warn-on-boxed)

(defn pentagonal-seq
  []
  (let [xf (comp
            (mapcat (juxt identity -))
            (map #(/ (* % (dec (* 3 %))) 2)))] ;; k (3k - 1) / 2
    (sequence xf (iterate inc 1))))

(def cached-pentagonal-seq
  (pentagonal-seq))

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

(defn prob-half-seq
  []
  (let [step (fn [[blue total]]
               [(+ (* 3 blue) (* 2 total) (- 2))
                (+ (* 4 blue) (* 3 total) (- 3))])]
    (iterate step [15 21])))

(defn reverse-digits
  [^long n]
  (loop [fwd  (quot n 10)
         bkwd (rem n 10)]
    (if (zero? fwd)
      bkwd
      (recur (quot fwd 10) (+ (* 10 bkwd) (rem fwd 10))))))

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

(defn totient
  [x]
  (long
   (reduce *
           (cons x
                 (for [p (dedupe (numeric/prime-factorize x))]
                   (- 1 (/ 1 p)))))))

(defn resilience
  [x]
  (/ (totient x) (dec x)))

(defn digits
  [^long x]
  (map #(Character/digit ^char % 10) (str x)))

(defn digital-sum
  ([x]
   (loop [x (long x)
          y 0]
     (if (zero? x)
       y
       (recur (quot x 10) (+ y (rem x 10)))))))

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

(defn kempner-seq
  [p]
  {:pre [numeric/prime? p]}
  (letfn [(step
            [[x k carry]]
            (if (pos? carry)
              [(* p x) k (dec carry)]
              [(* p x) (+ p k) (dec (numeric/int-log (+ p k) p))]))]
    (map pop (iterate step [p p 0]))))

(def solve-114
  (memoize
   (fn
     ([m n] (+ (solve-114 n (cons 1 (range m (inc n))) nil)))
     ([n choices prev]
      (cond
        (neg? n)         0
        (zero? n)        1
        (and prev
             (> prev 1)) (recur (dec n) choices 1)
        :else            (reduce + (for [i     choices
                                         :when (<= i n)]
                                     (solve-114 (- n i) choices i))))))))

(defn solve-110
  [])

(defn -main
  [& _]
  (with-open [reader (io/reader "resources/p107_network.txt")]
    (loop [edges   (read-edges reader)
           seen    #{}
           optimum #{}]
      (if-let [edge (first edges)]
        (if (every? #(contains? seen %) (:vertices edge))
          (recur (disj edges edge) seen optimum)
          (recur (disj edges edge)
                 (into seen (:vertices edge))
                 (conj optimum edge)))
        (reduce + (map :weight optimum))
        #_(into (sorted-set)
                (mapcat :vertices optimum))))))

(-main)
