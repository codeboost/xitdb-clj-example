(ns xitdb-clj-example.xitdb-util
  (:import
    (clojure.lang Associative)
    [io.github.radarroark.xitdb
     CoreFile CoreMemory Database$Float Database$Int Hasher Database
     Database$ContextFunction Database$Bytes Database$Uint
     RandomAccessMemory WriteArrayList WriteHashMap
     ReadArrayList ReadLinkedArrayList ReadHashMap Tag
     WriteCursor]
    [java.io File RandomAccessFile]
    [java.security MessageDigest]))


(defonce MAX_READ_BYTES 1024)

(declare map->WriteHashMap!)
(declare coll->WriteArrayList!)

(defn coll->WriteArrayList! [cursor coll]
  (let [write-array (WriteArrayList. cursor)]
    (doseq [v coll]
      (cond
        (map? v)
        (let [v-cursor (.appendCursor write-array)]
          (map->WriteHashMap! v-cursor v))

        (string? v)
        (.append write-array (Database$Bytes. v))

        (keyword? v)
        (.append write-array (Database$Bytes. (str v)))

        (integer? v)
        (.append write-array (Database$Uint. v))

        (sequential? v)
        (let [v-cursor (.appendCursor write-array)]
          (coll->WriteArrayList! v-cursor v))

        (boolean? v)
        (.append write-array (Database$Uint. (if v 1 0)))

        (instance? Database$Bytes v)
        (.append write-array v)

        (instance? Database$Uint v)
        (.append write-array v)

        :else
        (throw (IllegalArgumentException. (str "Unsupported type: " (type v))))))
    (.-cursor write-array)))

(defn primitive-for [v]
  (cond

    (string? v)
    (Database$Bytes. v)

    (keyword? v)
    (Database$Bytes. (str v))

    ;;TODO: Database$Int doesn't work (stores null)
    (integer? v)
    (Database$Uint. v)

    (boolean? v)
    (Database$Uint. (if v 1 0))

    (float? v)
    (Database$Float. v)

    :else
    (throw (IllegalArgumentException. (str "Unsupported type: " (type v))))))

(defn value-for! [cursor v & [keep?]]
  (cond
    (map? v)
    (do
      (when-not keep?
        (.write cursor nil)) ;; if there's anything, we clear it
      (.slot (map->WriteHashMap! cursor v)))

    (sequential? v)
    (do
      (when-not keep?
        (.write cursor nil))
      (.slot (coll->WriteArrayList! cursor v)))
    :else
    (primitive-for v)))

(defn array-list-assoc-value [wal i v]
  (when (> i (.count wal))
    (throw (IllegalArgumentException. "Index out of bounds")))

  (if (= i (.count wal))
    (.append wal (value-for! (.cursor wal) v))
    (let [cursor (.putCursor wal i)]
      (.write cursor (value-for! cursor v)))))

(defn assoc-value [whm k v]

  (let [k (str k)
        cursor (.putCursor whm k)]
    (.write cursor (value-for! cursor v true))

    #_(cond
        (map? v)
        (let [v-cursor (.putCursor whm k)]
          (map->WriteHashMap! v-cursor v))

        (sequential? v)
        (let [v-cursor (.putCursor whm k)]
          (coll->WriteArrayList! v-cursor v))

        :else
        (.put whm k (primitive-for v)))))

(defn writer-obj [cursor]
  (let [tag (-> cursor .slot .tag)]
    (cond
      (contains? #{Tag/NONE Tag/HASH_MAP} tag)
      (WriteHashMap. cursor)

      (= tag Tag/ARRAY_LIST)
      (WriteArrayList. cursor))))

(defn keypath-cursor [cursor ks]
  (loop [ks ks
         cursor cursor]
    (let [key (first ks)
          tag (-> cursor .slot .tag)]
      (if key
        (cond
          (contains? #{Tag/NONE Tag/HASH_MAP} tag)
          (recur (next ks) (.putCursor (WriteHashMap. cursor) (str key)))

          (= tag Tag/ARRAY_LIST)
          (recur (next ks) (let [array-list (WriteArrayList. cursor)]
                             (if (= key (.count array-list))
                               (.appendCursor array-list)
                               (.putCursor array-list key))))

          :else
          (throw (IllegalArgumentException. (str "Unsupported type: " tag))))
        cursor))))

(defn xitdb-assoc-in [cursor ks v]
  (let [cursor (keypath-cursor cursor ks)]
    (.write cursor (value-for! cursor v))))

(defn map->WriteHashMap! [cursor m]
  (let [whm (WriteHashMap. cursor)]
    (doseq [[k v] m]
      (assoc-value whm k v))
    (.-cursor whm)))

(defn WriteHashMap->map [cursor])

(defn v->xitdb [cursor v]
  (cond
    (map? v)
    (map->WriteHashMap! cursor v)

    (sequential? v)
    (coll->WriteArrayList! cursor v)

    :else
    (throw (IllegalArgumentException. (str "Value must be a map or a collection, not " (type v))))))

(defn open-db [filename]
  (let [core (if (= filename :memory)
               (CoreMemory. (RandomAccessMemory.))
               (CoreFile. (RandomAccessFile. (File. filename) "rw")))
        hasher (Hasher. (MessageDigest/getInstance "SHA-1"))]
    (Database. core hasher)))

(defn close-db! [db]
  (.close (.-core db)))


