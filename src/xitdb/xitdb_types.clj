(ns xitdb.xitdb-types
  (:require
    [xitdb.xitdb-util :as util])
  (:import
    [io.github.radarroark.xitdb ReadArrayList ReadCursor ReadHashMap Tag]))

(declare read-from-cursor)

(deftype XITDBArrayList [ral]
  clojure.lang.IPersistentCollection
  (seq [_]
    (util/array-seq ral read-from-cursor))

  (count [_]
    (.count ral))

  (cons [_ o]
    (throw (UnsupportedOperationException. "XITDBArrayList is read-only")))

  (empty [_]
    (throw (UnsupportedOperationException. "XITDBArrayList is read-only")))

  (equiv [this other]
    (and (sequential? other)
         (= (count this) (count other))
         (every? identity (map = this other))))

  clojure.lang.Sequential  ;; Add this to mark as sequential

  clojure.lang.Indexed
  (nth [_ i]
    (let [cursor (.getCursor ral (long i))]
      (read-from-cursor cursor)))

  (nth [_ i not-found]
    (try
      (let [cursor (.getCursor ral (long i))]
        (if cursor
          (read-from-cursor cursor)
          not-found))
      (catch Exception _
        not-found)))

  clojure.lang.ILookup
  (valAt [this k]
    (if (number? k)
      (.nth this (long k))
      (throw (IllegalArgumentException. "Key must be a number"))))

  (valAt [this k not-found]
    (if (number? k)
      (.nth this (long k) not-found)
      not-found))

  clojure.lang.IFn
  (invoke [this k]
    (.valAt this k))

  (invoke [this k not-found]
    (.valAt this k not-found))

  (applyTo [this args]
    (case (count args)
      1 (.invoke this (first args))
      2 (.invoke this (first args) (second args))
      (throw (IllegalArgumentException. "Wrong number of args passed to XITDBArrayList"))))

  clojure.lang.IReduceInit
  (reduce [this f init]
    (reduce f init (util/array-seq ral read-from-cursor)))

  java.util.Collection
  (^"[Ljava.lang.Object;" toArray [this]
    (to-array (into [] this)))

  (^"[Ljava.lang.Object;" toArray [this ^"[Ljava.lang.Object;" array]
    (let [len (count this)
          result (if (or (nil? array) (< (alength array) len))
                   (make-array Object len)
                   array)]
      (dotimes [i len]
        (aset result i (nth this i)))
      (when (> (alength result) len)
        (aset result len nil))
      result))
  Object
  (toString [this]
    (pr-str (into [] this))))

(defmethod print-method XITDBArrayList [o ^java.io.Writer w]
  (.write w "#XITDBArrayList")
  (print-method (into [] o) w))


(deftype XITDBHashMap [rhm]
  clojure.lang.ILookup
  (valAt [this key]
    (.valAt this key nil))

  (valAt [this key not-found]
    (let [cursor (.getCursor rhm (util/db-key key))]
      (if (nil? cursor)
        not-found
        (read-from-cursor cursor))))

  clojure.lang.Associative
  (containsKey [this key]
    (not (nil? (.getCursor rhm (util/db-key key)))))

  (entryAt [this key]
    (let [v (.valAt this key nil)]
      (when-not (nil? v)
        (clojure.lang.MapEntry. key v))))

  (assoc [this k v]
    (throw (UnsupportedOperationException. "XITDBHashMap is read-only")))

  clojure.lang.IPersistentMap
  (without [_ _]
    (throw (UnsupportedOperationException. "XITDBHashMap is read-only")))

  (count [this]
    (.valAt this (util/internal-keys :count) 0))

  clojure.lang.IPersistentCollection
  (cons [_ _]
    (throw (UnsupportedOperationException. "XITDBHashMap is read-only")))

  (empty [_]
    (throw (UnsupportedOperationException. "XITDBHashMap is read-only")))

  (equiv [this other]
    (and (instance? clojure.lang.IPersistentMap other)
         (= (into {} this) (into {} other))))

  clojure.lang.Seqable
  (seq [this]
    (util/map-seq rhm read-from-cursor))

  clojure.lang.IFn
  (invoke [this k]
    (.valAt this k))

  (invoke [this k not-found]
    (.valAt this k not-found))

  java.lang.Iterable
  (iterator [this]
    (let [iter (clojure.lang.SeqIterator. (seq this))]
      (reify java.util.Iterator
        (hasNext [_]
          (.hasNext iter))
        (next [_]
          (.next iter))
        (remove [_]
            (throw (UnsupportedOperationException. "XITDBHashMap iterator is read-only"))))))

  Object
  (toString [this]
    (str (into {} this))))

(defmethod print-method XITDBHashMap [o ^java.io.Writer w]
  (.write w "#XITDBHashMap")
  (print-method (into {} o) w))

(defn read-from-cursor [^ReadCursor cursor]
  (let [value-tag (some-> cursor .slot .tag)]
    (cond
      (contains? #{Tag/SHORT_BYTES Tag/BYTES} value-tag)
      (util/read-bytes-with-format-tag cursor)

      (= value-tag Tag/UINT)
      (.readUint cursor)

      (= value-tag Tag/INT)
      (.readInt cursor)

      (= value-tag Tag/FLOAT)
      (.readFloat cursor)

      (= value-tag Tag/HASH_MAP)
      (XITDBHashMap. (ReadHashMap. cursor))

      (= value-tag Tag/ARRAY_LIST)
      (XITDBArrayList. (ReadArrayList. cursor))

      :else
      nil)))