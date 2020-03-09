(ns com.github.hindol.euler.modular
  (:refer-clojure :exclude [+ *])
  (:require
   [clojure.tools.trace :as trace]))

(set! *warn-on-reflection* :warn-on-boxed)

(def ^:dynamic *modulus* 1000000009)

(defn +
  ^long [^long x ^long y]
  (rem (clojure.core/+' x y) *modulus*))

(defn *
  ^long [^long x ^long y]
  (rem (clojure.core/*' x y) *modulus*))

(defn factorial
  [^long n]
  {:pre [(not (neg? n))]}
  (cond
    (#{0 1} n) 1
    :else      (loop [i 3
                      f 2]
                 (if (> i n)
                   f
                   (recur (inc i) (* f i))))))

(defn expt
  "Returns x raised to e."
  [^long x ^long e]
  (cond
    (neg? e)  (/ (expt x (- e)))
    (zero? e) 1
    (pos? e)  (if (even? e)
                (recur (* x x)
                       (quot e 2))
                (* x (expt x (dec e))))))