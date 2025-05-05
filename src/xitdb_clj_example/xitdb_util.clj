(ns xitdb-clj-example.xitdb-util
  (:import
    (clojure.lang Associative)
    [io.github.radarroark.xitdb
     Database$Float Database$Bytes Database$Uint
     WriteArrayList WriteHashMap Tag]))

(defn print-tag [tag]
  (cond
    (= tag Tag/NONE) :none
    (= tag Tag/INDEX) :index
    (= tag Tag/ARRAY_LIST) :array-list
    (= tag Tag/LINKED_ARRAY_LIST) :linked-array-list
    (= tag Tag/HASH_MAP) :hash-map
    (= tag Tag/KV_PAIR) :kv-pair
    (= tag Tag/BYTES) :bytes
    (= tag Tag/SHORT_BYTES) :short-bytes
    (= tag Tag/UINT) :uint
    (= tag Tag/INT) :int
    (= tag Tag/FLOAT) :float
    :else :unknown))

(declare map->WriteHashMap!)
(declare coll->WriteArrayList!)

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

(defn coll->WriteArrayList! [cursor coll]
  (let [write-array (WriteArrayList. cursor)]
    (doseq [v coll]
      (cond
        (map? v)
        (let [v-cursor (.appendCursor write-array)]
          (map->WriteHashMap! v-cursor v))

        (coll? v)
        (let [v-cursor (.appendCursor write-array)]
          (coll->WriteArrayList! v-cursor v))

        :else
        (.append write-array (primitive-for v))))
    (.-cursor write-array)))

(defn value-for! [cursor v]
  (cond

    (instance? WriteArrayList v)
    (-> v .-cursor .slot)

    (instance? WriteHashMap v)
    (-> v .-cursor .slot)

    (map? v)
    (do
      (when-not (= Tag/HASH_MAP (-> cursor .slot .tag))
        (.write cursor nil))
      (.slot (map->WriteHashMap! cursor v)))

    (coll? v)
    (do
      (when-not (= Tag/ARRAY_LIST (-> cursor .slot .tag))
        (.write cursor nil))

      (.slot (coll->WriteArrayList! cursor v)))
    :else
    (primitive-for v)))

(defn array-list-assoc-value [wal i v]

  (assert (= Tag/ARRAY_LIST (-> wal .cursor .slot .tag)))
  (assert (number? i))

  (when (> i (.count wal))
    (throw (IllegalArgumentException. "Index out of bounds")))

  (let [cursor (if (= i (.count wal))
                 (.appendCursor wal)
                 (.putCursor wal i))]
    (.write cursor (value-for! cursor v))))

(defn map-assoc-value [whm k v]
  (let [k (str k)
        cursor (.putCursor whm k)]
    (.write cursor (value-for! cursor v))))

(defn map->WriteHashMap! [cursor m]
  (let [whm (WriteHashMap. cursor)]
    (doseq [[k v] m]
      (map-assoc-value whm k v))
    (.-cursor whm)))



