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

;; map of logical tag -> string used as formatTag in the Bytes record.
(def fmt-tag-value
  {:keyword     "kw"
   :boolean     "bl"
   :key-integer "ki"
   :nil         "nl"
   :inst        "in"
   :date        "da"})

;; map of logical key -> key stored in the HashMap
(def internal-keys
  {:count :%xitdb__count})

;; HashMap keys which are used internally and should be hidden from user
(def hidden-keys (set (vals internal-keys)))

(declare map->WriteHashMapCursor!)
(declare coll->ArrayListCursor!)

(defn keyname [key]
  (if (keyword? key)
    (if (namespace key)
      (str (namespace key) "/" (name key))
      (name key))
    key))

(let [d (java.util.Date.)
      sd (str (.toInstant d))
      parsed-inst (java.time.Instant/parse sd)]
  (instance? java.time.Instant parsed-inst))

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

    (nil? v)
    (Database$Bytes. "" (fmt-tag-value :nil))

    (instance? java.time.Instant v)
    (Database$Bytes. (str v) (fmt-tag-value :inst))

    (instance? java.util.Date v)
    (Database$Bytes. (str (.toInstant v)) (fmt-tag-value :date))

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

(defn db-key
  "Converts k from a Clojure type to a Database$Bytes representation to be used in
  cursor functions."
  [k]
  (cond
    (integer? k)
    (Database$Bytes. (str k) "ki") ;integer keys are stored as strings with 'ki' format tag
    :else
    (primitive-for k)))

(defn update-map-item-count! [whm f]
  (let [existing (.getCursor whm (db-key (internal-keys :count)))]
    (let [cursor (.putCursor whm (db-key (internal-keys :count)))]
      (if existing
        (.write cursor (primitive-for (f (.readInt cursor))))
        (.write cursor (primitive-for 1))))))

(defn map-dissoc-key!
  [whm k]

  (when (contains? hidden-keys k)
    (throw (IllegalArgumentException. (str "Cannot dissoc key. " k ". It is reserved for internal use."))))

  (when (.remove whm (db-key k))
    (update-map-item-count! whm dec)))

(defn map-assoc-value!
  "Associates a key-value pair in a WriteHashMap.
  Converts the key to a string and the value to an appropriate XitDB representation."
  [whm k v]
  (when (contains? hidden-keys k)
    (throw (IllegalArgumentException. (str "Cannot assoc key. " k ". It is reserved for internal use."))))

  (let [existing (.getCursor whm (db-key k))
        cursor (.putCursor whm (db-key k))]
    (.write cursor (v->slot! cursor v))
    (when existing
      (update-map-item-count! whm inc))
    whm))

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

      (= fmt-tag (fmt-tag-value :inst))
      (java.time.Instant/parse str)


      (= fmt-tag (fmt-tag-value :date))
      (java.util.Date/from
        (java.time.Instant/parse str))


      (= fmt-tag (fmt-tag-value :nil))
      nil

      :else
      str)))

(defn map-seq
  "Iterates through a ReadHashMap or WriteHashMap.
  `read-from-cursor` is a function which reads the data and converts
  it into the proper XITDBHashMap or XITDBWriteHashMap types.
  Returns a lazy sequence of key-value entries."
  [rhm read-from-cursor]
  (let [iterator (.iterator rhm)
        lazy-iter (fn lazy-iter []
                    (lazy-seq
                      (when (.hasNext iterator)
                        (let [cursor (.next iterator)
                              kv-pair (.readKeyValuePair cursor)
                              key-cursor (.-keyCursor kv-pair)
                              key (read-bytes-with-format-tag key-cursor)]
                          (if (contains? hidden-keys key)
                            ;; Skip hidden keys by recursing
                            (lazy-iter)
                            ;; Return this entry and the rest of the sequence
                            (let [value-cursor (.-valueCursor kv-pair)
                                  value (read-from-cursor value-cursor)
                                  entry (clojure.lang.MapEntry. key value)]
                              (cons entry (lazy-iter))))))))]
    (lazy-iter)))




