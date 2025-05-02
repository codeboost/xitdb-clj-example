(ns xitdb-clj-example.xitdb-util
  (:import
    (clojure.lang Associative)
    [io.github.radarroark.xitdb
     CoreFile CoreMemory Hasher Database
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
  (println "writing array list: " coll)
  (let [cursor (WriteArrayList. cursor)]
    (doseq [v coll]
      (cond
        (map? v)
        (let [v-cursor (.appendCursor cursor)]
          (map->WriteHashMap! v-cursor v))

        (string? v)
        (.append cursor (Database$Bytes. v))

        (keyword? v)
        (.append cursor (Database$Bytes. (str v)))

        (integer? v)
        (.append cursor (Database$Uint. v))

        (sequential? v)
        (let [v-cursor (.appendCursor cursor)]
          (coll->WriteArrayList! v-cursor v))

        (boolean? v)
        (.append cursor (Database$Uint. (if v 1 0)))

        (instance? Database$Bytes v)
        (.append cursor v)

        (instance? Database$Uint v)
        (.append cursor v)

        :else
        (throw (IllegalArgumentException. (str "Unsupported type: " (type v)))))))
  cursor)

(defn map->WriteHashMap! [cursor m]
  (println "Writing hashmap: " m)
  (let [cursor (WriteHashMap. cursor)]
    (doseq [[k v] m]
      (let [k (str k)]

        (.putKey cursor k (Database$Bytes. k))

        (cond
          (map? v)
          (let [v-cursor (.putCursor cursor k)]
            (map->WriteHashMap! v-cursor v))

          (sequential? v)
          (let [v-cursor (.putCursor cursor k)]
            (coll->WriteArrayList! v-cursor v))

          (string? v)
          (.put cursor k (Database$Bytes. v))

          (keyword? v)
          (.put cursor k (Database$Bytes. (str v)))

          (integer? v)
          (.put cursor k (Database$Uint. v))

          (boolean? v)
          (.put cursor k (Database$Uint. (if v 1 0)))

          :else
          (throw (IllegalArgumentException. (str "Unsupported type: " (type v))))))))
  cursor)

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


