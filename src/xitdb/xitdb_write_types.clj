(ns xitdb.xitdb-write-types
  (:require
    [xitdb.xitdb-util :as util])
  (:import
    (io.github.radarroark.xitdb Tag WriteArrayList WriteHashMap)))

(declare read-from-cursor unwrap)

(deftype XITDBWriteArrayList [wal]
  clojure.lang.IPersistentCollection
  (count [this]
    (.count wal))

  (cons [this o]
    ;;TODO: Figure out if it is correct to append to the end
    (util/array-list-assoc-value! wal (.count wal) (unwrap o))
    this)

  (empty [this]
    #_(XITDBWriteArrayList. wal))

  (equiv [this other]
    (if (instance? XITDBWriteArrayList other)
      (and (= (count this) (count other))
           (every? (fn [i] (= (get this i) (get other i)))
                   (range (count this))))
      false))

  clojure.lang.Indexed
  (nth [this i]
    (.nth this i nil))

  (nth [this i not-found]
    (if (and (>= i 0) (< i (.count wal)))
      (let [ret (read-from-cursor (.putCursor wal i))]
        #_(println "read-from-cursor " i (count ret))
        ret)
      not-found))

  clojure.lang.Associative
  (assoc [this k v]
    (when-not (integer? k)
      (throw (IllegalArgumentException. "Key must be integer")))
    (util/array-list-assoc-value! wal k (unwrap v))
    this)

  (containsKey [this k]
    (and (integer? k) (>= k 0) (< k (.count wal))))

  (entryAt [this k]
    (when (.containsKey this k)
      (clojure.lang.MapEntry. k (.valAt this k))))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))

  (valAt [this k not-found]
    (.nth this k not-found))

  clojure.lang.Seqable
  (seq [this]
    (when (> (.count wal) 0)
      (map #(.valAt this %) (range (.count wal)))))

  Object
  (toString [this]
    (str "XITDBWriteArrayList")))

;;---------------------------------------


(deftype XITDBWriteHashMap [whm]
  clojure.lang.IPersistentCollection
  (cons [this o]

    (cond
      (instance? clojure.lang.MapEntry o)
      (.assoc this (key o) (val o))

      (map? o)
      (let [[k v] (first o)]
        (.assoc this k v))

      (and (sequential? o) (= 2 (count o)))
      (.assoc this (first o) (second o))

      :else
      (throw (IllegalArgumentException. "Can only cons MapEntries or key-value pairs onto maps")))
    this)

  (empty [this]
    (throw (IllegalArgumentException. "empty not implemented")))

  (equiv [this other]
    (and (= (count this) (count other))
         (every? (fn [[k v]] (= v (get other k ::not-found)))
                 (seq this))))
  clojure.lang.Associative
  (assoc [this k v]
    (util/map-assoc-value! whm k (unwrap v))
    this)

  (containsKey [this key]
    (not (nil? (.putCursor whm (util/write-key key)))))

  (entryAt [this key]
    (let [cursor (.putCursor whm (util/write-key key))]
      (when (some? cursor)
        (clojure.lang.MapEntry. key (read-from-cursor cursor)))))

  clojure.lang.IPersistentMap
  (without [this key]
    (.remove whm (util/keyname key))
    this)

  (count [this]
    (let [iter (.iterator whm)]
      (loop [count 0]
        (if (.hasNext iter)
          (do (.next iter) (recur (inc count)))
          count))))


  clojure.lang.ILookup
  (valAt [this key]
    (.valAt this key nil))

  (valAt [this key not-found]
    (let [cursor (.getCursor whm (util/keyname key))]
      (if (nil? cursor)
        not-found
        (read-from-cursor (.putCursor whm (util/write-key key))))))

  clojure.lang.Seqable
  (seq [this]
    (util/map-seq whm read-from-cursor))

  Object
  (toString [this]
    (str "XITDBWriteHashMap")))

(defn read-from-cursor [cursor]
  (let [value-tag (some-> cursor .slot .tag)]
    #_(println "value-tag:" (util/xit-tag->keyword value-tag))
    (cond
      (contains? #{Tag/SHORT_BYTES Tag/BYTES} value-tag)
      (util/read-bytes-with-format-tag cursor)

      (= value-tag Tag/UINT)
      (.readUint cursor)

      (= value-tag Tag/INT)
      (.readInt cursor)

      (= value-tag Tag/HASH_MAP)
      (XITDBWriteHashMap. (WriteHashMap. cursor))

      (= value-tag Tag/ARRAY_LIST)
      (XITDBWriteArrayList. (WriteArrayList. cursor))

      :else
      nil)))

(defn unwrap [v]
  (cond
    (instance? XITDBWriteArrayList v)
    (.-wal v)

    (instance? XITDBWriteHashMap v)
    (.-whm v)

    :else
    v))

(defn slot-for-value! [cursor v]
  (cond
    (instance? XITDBWriteArrayList v)
    (-> v .wal .cursor .slot)

    (instance? XITDBWriteHashMap v)
    (-> v .whm .cursor .slot)
    :else
    (util/v->slot! cursor v)))
