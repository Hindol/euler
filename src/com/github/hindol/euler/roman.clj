(ns com.github.hindol.euler.roman
  (:require
   [clojure.set :as s]))

(def ^:private ^:const ->hindu-arabic
  {\I         1
   \V         5
   \X         10
   \L         50
   \C         100
   \D         500
   \M         1000
   [\I \V] 4
   [\I \X] 9
   [\X \L] 40
   [\X \C] 90
   [\C \D] 400
   [\C \M] 900})

(def ^:private ^:const ->roman
  (s/map-invert ->hindu-arabic))

(defn roman-tokenize
  [[x & more]]
  (lazy-seq
   (let [y (first more)
         v (->hindu-arabic [x y])]
     (cond
       (and y v) (cons [x y] (roman-tokenize (rest more)))
       x         (cons x (roman-tokenize more))))))

(defn roman->hindu-arabic
  [s]
  (->> s
       roman-tokenize
       (map ->hindu-arabic)
       (reduce +)))

(defn hindu-arabic-tokenize
  ([x] (hindu-arabic-tokenize x 1))
  ([x p]
   (lazy-seq
    (when (pos? x)
      (let [d (rem x 10)]
        (cond
          (zero? d)            (hindu-arabic-tokenize (quot x 10) (* 10 p))
          (and (< p 1000)
               (#{1 4 5 9} d)) (cons (* d p)
                                     (hindu-arabic-tokenize (quot x 10) (* 10 p)))
          :else                (cons p
                                     (hindu-arabic-tokenize (dec x) p))))))))

(defn hindu-arabic->roman
  [x]
  (->> x
       hindu-arabic-tokenize
       (map ->roman)
       reverse
       flatten
       (apply str)))

(defn minimal-roman
  [s]
  (-> s
      roman->hindu-arabic
      hindu-arabic->roman))