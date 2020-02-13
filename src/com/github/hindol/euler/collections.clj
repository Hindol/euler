(ns com.github.hindol.euler.collections
  (:import
   (clojure.lang IPersistentCollection
                 IPersistentSet
                 Seqable)))

(deftype Bag [^clojure.lang.IPersistentMap m
              ^long n]
  IPersistentSet
  (get [this k]
    (if (contains? m k) k nil))
  (contains [this k]
    (contains? m k))
  (disjoin [this k]
    (Bag. (if (= 1 (m k)) (dissoc m k) (update m k dec))
          (dec n)))

  IPersistentCollection
  (count [this]
    n)
  (empty [this]
    (Bag. (.empty m) 0))
  (cons [this k]
    (Bag. (assoc m k (inc (get m k 0)))
          (inc n)))
  (equiv [this o]
    (and (isa? (class o) Bag)
         (= n (.n ^Bag o))
         (.equiv m (.m ^Bag o))))

  Seqable
  (seq [this] (mapcat repeat (vals m) (keys m))))

(defn bag
  [& keys]
  (Bag. (frequencies keys)
        (count keys)))