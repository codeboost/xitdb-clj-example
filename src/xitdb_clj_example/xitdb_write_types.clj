(ns xitdb-clj-example.xitdb-write-types
  (:require
    [xitdb-clj-example.xitdb-types :as types]
    [xitdb-clj-example.xitdb-util :as util])
  (:import
    (clojure.lang Associative IReduceInit)
    [io.github.radarroark.xitdb
     CoreFile CoreMemory Hasher Database
     Database$ContextFunction Database$Bytes Database$Uint
     RandomAccessMemory WriteArrayList WriteHashMap
     ReadArrayList ReadLinkedArrayList ReadHashMap Tag
     WriteCursor]
    [java.io File RandomAccessFile]
    [java.security MessageDigest]))

(declare read-from-cursor)

(deftype XITDBWriteArrayList [wal]
  clojure.lang.IPersistentCollection
  (count [this]
    (.length wal))

  (cons [this o]
    (util/array-list-assoc-value wal (.count wal) o)
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
    (try
      (if (and (>= i 0) (< i (.length wal)))
        (read-from-cursor (.getCursor wal i))
        not-found)
      (catch Exception _ not-found)))

  clojure.lang.Associative
  (assoc [this k v]
    (when-not (integer? k)
      (throw (IllegalArgumentException. "Key must be integer")))
    (util/array-list-assoc-value wal k v)
    this)

  (containsKey [this k]
    (try
      (and (integer? k) (>= k 0) (< k (.length wal)))
      (catch Exception _ false)))

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
    (when (> (.length wal) 0)
      (map #(.valAt this %) (range (.length wal)))))

  Object
  (toString [this]
    (str "XITDBWriteArrayList")))

(deftype XITDBWriteHashMap [whm]
  clojure.lang.Associative
  (assoc [this k v]
    (util/map-assoc-value whm k v)
    this)

  (containsKey [this key]
    (try
      (not (nil? (.getCursor whm (str key))))
      (catch Exception _ false)))

  (entryAt [this key]
    (let [cursor (.getCursor whm (str key))]
      (when (some? cursor)
        (clojure.lang.MapEntry. key (read-from-cursor cursor)))))

  clojure.lang.IPersistentMap
  (without [this k]
    (.remove whm (str k))
    this)

  clojure.lang.ILookup
  (valAt [this key]
    (.valAt this key nil))

  (valAt [this key not-found]
    (try
      (let [cursor (.getCursor whm (str key))]
        (if (nil? cursor)
          not-found
          (read-from-cursor cursor)))
      (catch Exception _ not-found)))

  Object
  (toString [this]
    (str "XITDBWriteHashMap")))

(defn read-from-cursor [cursor]
  (let [value-tag (some-> cursor .slot .tag)]
    (cond
      (contains? #{Tag/SHORT_BYTES Tag/BYTES} value-tag)
      (let [s (String. (.readBytes cursor nil))]
        (if (.startsWith s ":")
          (keyword (.substring s 1))
          s))

      (= value-tag Tag/UINT)
      (.readUint cursor)

      (= value-tag Tag/HASH_MAP)
      (XITDBWriteHashMap. (WriteHashMap. cursor))

      (= value-tag Tag/ARRAY_LIST)
      (XITDBWriteArrayList. (WriteArrayList. cursor))

      :else
      nil)))