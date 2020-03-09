(ns com.github.hindol.euler.graph
  (:require
   [clojure.java.io :as io]
   [com.github.hindol.euler.matrix :as matrix]))

(defn vertices
  [matrix]
  (range (count matrix)))

(defn edges
  ([matrix]
   (for [[i row]    (map-indexed vector matrix)
         [j weight] (map-indexed vector row)
         :when      (and weight (< i j))]
     {:weight   weight
      :vertices #{i j}}))
  ([matrix i]
   (for [[j weight] (map-indexed vector (matrix i))
         :when      weight]
     {:weight weight
      :vertex j}))
  ([matrix i j]
   (when-let [weight (get-in matrix [i j])]
     {:weight weight})))

(with-open [reader (io/reader "resources/p107_network.txt")]
  (let [->int #(when (not= % "-")
                 (Integer/parseInt %))
        graph (matrix/read-str reader {:value-fn ->int})]
    (edges graph 0 2)))