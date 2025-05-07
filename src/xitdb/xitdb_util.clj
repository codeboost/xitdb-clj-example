(ns xitdb.xitdb-util
  (:import
    [io.github.radarroark.xitdb Database$Float Database$Bytes Database$Int Database$Uint WriteArrayList WriteHashMap Tag]))

(defn xit-tag->keyword
  "Converts a XitDB Tag enum to a corresponding Clojure keyword."
  [tag]
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

(def fmt-tag-value
  {:keyword "kw"
   :boolean "bl"
   :key-integer "ki"})

(declare map->WriteHashMapCursor!)
(declare coll->ArrayListCursor!)

(defn keyname [key]
  (if (keyword? key)
    (if (namespace key)
      (str (namespace key) "/" (name key))
      (name key))
    key))

(defn primitive-for
  "Converts a Clojure primitive value to its corresponding XitDB representation.
  Handles strings, keywords, integers, booleans, and floats.
  Throws an IllegalArgumentException for unsupported types."
  [v]
  (cond

    (string? v)
    (Database$Bytes. ^String v)

    (keyword? v)
    (Database$Bytes. (keyname v) (fmt-tag-value :keyword))


    (integer? v)
    (Database$Int. v)

    (boolean? v)
    (Database$Bytes. (if v "#t" "#f") (fmt-tag-value :boolean))

    (double? v)
    (Database$Float. v)

    :else
    (throw (IllegalArgumentException. (str "Unsupported type: " (type v) v)))))

(defn v->slot!
  "Converts a value to a XitDB slot.
  Handles WriteArrayList and WriteHashMap instances directly.
  Recursively processes Clojure maps and collections.
  Falls back to primitive conversion for other types."
  [cursor v]
  (cond

    (instance? WriteArrayList v)
    (-> v .-cursor .slot)

    (instance? WriteHashMap v)
    (-> v .-cursor .slot)

    (map? v)
    (do
      (.write cursor nil)
      (.slot (map->WriteHashMapCursor! cursor v)))

    (coll? v)
    (do
      (.write cursor nil)
      (.slot (coll->ArrayListCursor! cursor v)))
    :else
    (primitive-for v)))

(defn array-list-assoc-value!
  "Associates a value at index i in a WriteArrayList.
  Appends the value if the index equals the current count.
  Replaces the value at the specified index otherwise.
  Throws an IllegalArgumentException if the index is out of bounds."
  [wal i v]

  (assert (= Tag/ARRAY_LIST (-> wal .cursor .slot .tag)))
  (assert (number? i))

  (when (> i (.count wal))
    (throw (IllegalArgumentException. "Index out of bounds. ")))

  (let [cursor (if (= i (.count wal))
                 (.appendCursor wal)
                 (.putCursor wal i))]
    (.write cursor (v->slot! cursor v))))

(defn read-key
  "Returns the key to be used in a ReadHashMap.getCursor(key) call."
  [k]
  (cond
    (integer? k)
    (str k) ;integer keys are stored as strings with 'ki' format tag
    :else
    (keyname k)))

(defn write-key
  "Returns the key to be written to the database by WriteHashMap.putCursor()."
  [k]
  (cond
    (integer? k)
    (Database$Bytes. (str k) "ki") ;integer keys are stored as strings with 'ki' format tag
    :else
    (primitive-for k)))

(defn map-assoc-value!
  "Associates a key-value pair in a WriteHashMap.
  Converts the key to a string and the value to an appropriate XitDB representation."
  [whm k v]
  (let [cursor (.putCursor whm (write-key k))]
    (.write cursor (v->slot! cursor v))))

(defn coll->ArrayListCursor!
  "Converts a Clojure collection to a XitDB ArrayList cursor.
  Handles nested maps and collections recursively.
  Returns the cursor of the created WriteArrayList."
  [cursor coll]
  (let [write-array (WriteArrayList. cursor)]
    (doseq [v coll]
      (cond
        (map? v)
        (let [v-cursor (.appendCursor write-array)]
          (map->WriteHashMapCursor! v-cursor v))

        (coll? v)
        (let [v-cursor (.appendCursor write-array)]
          (coll->ArrayListCursor! v-cursor v))

        :else
        (.append write-array (primitive-for v))))
    (.-cursor write-array)))

(defn map->WriteHashMapCursor!
  "Writes a Clojure map to a XitDB WriteHashMap.
  Returns the cursor of the created WriteHashMap."
  [cursor m]
  (let [whm (WriteHashMap. cursor)]
    (doseq [[k v] m]
      (map-assoc-value! whm k v))
    (.-cursor whm)))

(defn key-tag-valid?
  "Checks if a key cursor has a valid tag type (bytes or short bytes)."
  [key-cursor]
  ;;TODO: Can the key have other types ?
  (contains? #{Tag/BYTES Tag/SHORT_BYTES} (-> key-cursor .slot .tag)))

(defn read-bytes-with-format-tag [cursor]
  (let [bytes-obj (.readBytesObject cursor nil)
        str (String. (.value bytes-obj))
        fmt-tag (some-> bytes-obj .formatTag String.)]
    (cond
      (= fmt-tag (fmt-tag-value :keyword))
      (keyword str)

      (= fmt-tag (fmt-tag-value :boolean))
      (= str "#t")

      (= fmt-tag (fmt-tag-value :key-integer))
      (Integer/parseInt str)

      :else
      str)))

(defn map-seq
  "Iterates through a ReadHashMap or WriteHashMap.
  `read-from-cursor` is a function which reads the data and converts
  it into the proper XITDBHashMap or XITDBWriteHashMap types."
  [rhm read-from-cursor]
  (let [iterator (.iterator rhm)]
    (loop [entries []
           has-next (.hasNext iterator)]
      (if has-next
        (let [cursor (.next iterator)
              kv-pair (.readKeyValuePair cursor)
              key-cursor (.-keyCursor kv-pair)]
          (let [key (read-bytes-with-format-tag key-cursor)
                value-cursor (.-valueCursor kv-pair)
                value (read-from-cursor value-cursor)]
            (recur (conj entries (clojure.lang.MapEntry. key value))
                 (.hasNext iterator))))
        (seq entries)))))




