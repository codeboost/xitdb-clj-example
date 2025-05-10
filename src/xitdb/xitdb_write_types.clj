(ns xitdb.xitdb-write-types
  (:require
    [xitdb.xitdb-util :as util])
  (:import
    (io.github.radarroark.xitdb Slot Tag WriteArrayList WriteCursor WriteHashMap)))

(declare read-from-cursor unwrap)
(declare ->XITDBWriteArrayList)

(deftype XITDBWriteArrayList [^WriteArrayList wal]
  clojure.lang.IPersistentCollection
  (count [this]
    (.count wal))

  (cons [this o]
    ;;TODO: Figure out if it is correct to append to the end
    (util/array-list-assoc-value! wal (.count wal) (unwrap o))
    this)

  (empty [this]
    (util/array-list-empty! wal)
    this)

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
      (read-from-cursor (.putCursor wal i))
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
    (util/array-seq wal read-from-cursor))

  clojure.lang.IObj
  (withMeta [this _]
    this)

  clojure.lang.IMeta
  (meta [this]
    nil)

  clojure.lang.IEditableCollection
  (asTransient [this]
    this)

  clojure.lang.ITransientCollection
  (conj [this val]
    (util/array-list-append-value! wal (unwrap val))
    this)

  (persistent [this]
    this)

  clojure.lang.ITransientVector ;; assoc already implemented

  (pop [this]
    (let [value (read-from-cursor (-> wal .-cursor))]
      (util/array-list-pop! wal)
      value))

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
      (doseq [[k v] (seq o)]
        (.assoc this k v))

      (and (sequential? o) (= 2 (count o)))
      (do
        (.assoc this (first o) (second o)))

      :else
      (throw (IllegalArgumentException. "Can only cons MapEntries or key-value pairs onto maps")))
    this)

  (empty [this]
    (util/map-empty! whm)
    this)

  (equiv [this other]
    (and (= (count this) (count other))
         (every? (fn [[k v]] (= v (get other k ::not-found)))
                 (seq this))))
  clojure.lang.Associative
  (assoc [this k v]
    (util/map-assoc-value! whm k (unwrap v))
    this)

  (containsKey [this key]
    (util/map-contains-key? whm key))

  (entryAt [this key]
    (when (.containsKey this key)
      (clojure.lang.MapEntry. key (.valAt this key))))

  clojure.lang.IPersistentMap
  (without [this key]
    (util/map-dissoc-key! whm key)
    this)

  (count [this]
    (util/map-item-count whm))

  clojure.lang.ILookup
  (valAt [this key]
    (.valAt this key nil))

  (valAt [this key not-found]
    (let [cursor (util/map-read-cursor whm key)]
      (if (nil? cursor)
        not-found
        (read-from-cursor (util/map-write-cursor whm key)))))

  clojure.lang.Seqable
  (seq [this]
    (util/map-seq whm read-from-cursor))

  Object
  (toString [this]
    (str "XITDBWriteHashMap")))

(defn read-from-cursor [^WriteCursor cursor]
  (let [value-tag (some-> cursor .slot .tag)]
    #_(println "value-tag:" (util/xit-tag->keyword value-tag))
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
      (XITDBWriteHashMap. (WriteHashMap. cursor))

      (= value-tag Tag/ARRAY_LIST)
      (XITDBWriteArrayList. (WriteArrayList. cursor))

      :else
      nil)))

(defn unwrap [v]
  (cond
    (instance? XITDBWriteArrayList v)
    (.-wal ^XITDBWriteArrayList v)

    (instance? XITDBWriteHashMap v)
    (.-whm ^XITDBWriteHashMap v)

    :else
    v))

(defn ^Slot slot-for-value! [^WriteCursor cursor v]
  (cond
    (instance? XITDBWriteArrayList v)
    (let [^WriteArrayList wal  (.-wal ^XITDBWriteArrayList v)]
      (-> wal .cursor .slot))

    (instance? XITDBWriteHashMap v)
    (let [^WriteHashMap whm (.-whm ^XITDBWriteHashMap v)]
      (-> whm .cursor .slot))

    :else
    (util/v->slot! cursor v)))
