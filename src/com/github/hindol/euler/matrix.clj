(ns com.github.hindol.euler.matrix
  (:require
   [clojure.string :as str]))

(defn read-str
  ([reader] (read-str reader identity))
  ([reader {value-fn :value-fn}]
   (->> reader
        line-seq
        (mapv #(->> (str/split % #",")
                    (mapv value-fn))))))