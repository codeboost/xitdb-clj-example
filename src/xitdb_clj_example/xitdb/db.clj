(ns xitdb-clj-example.xitdb.db
  (:require
    [xitdb-clj-example.xitdb-types :as xtypes]

    [xitdb-clj-example.xitdb-write-types :as wtypes]
    [xitdb-clj-example.xitdb-util :as util])
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

(defn db-history [db]
  (WriteArrayList. (.rootCursor db)))

(defn append-context [history fn]
  (.appendContext
    history
    (.getSlot history -1)
    (reify Database$ContextFunction
      (^void run [_ ^WriteCursor cursor]
        (fn cursor)
        nil))))

(defn v->xitdb! [cursor v]
  (cond
    (map? v)
    (util/map->WriteHashMap! cursor v)

    (sequential? v)
    (util/coll->WriteArrayList! cursor v)

    :else
    (throw (IllegalArgumentException. (str "Value must be a map or a collection, not " (type v))))))

(defn xitdb-reset! [history new-value]
  (.appendContext
    history
    nil
    (reify Database$ContextFunction
      (^void run [_ ^WriteCursor cursor]
        (v->xitdb! cursor new-value)
        nil))))

(defn xitdb-read [history]
  (let [cursor (.getCursor history -1)]
    (xtypes/read-from-cursor cursor)))

(defn open-database [filename]
  (let [core (if (= filename :memory)
               (CoreMemory. (RandomAccessMemory.))
               (CoreFile. (RandomAccessFile. (File. filename) "rw")))
        hasher (Hasher. (MessageDigest/getInstance "SHA-1"))]
    (Database. core hasher)))

(defn close-db! [db]
  (.close (.-core db)))

(defn xitdb-swap! [db f & args]
  (let [history (db-history db)]
    (append-context history (fn [cursor]
                              (let [tag (-> cursor .slot .tag)
                                    obj (cond
                                          (contains? #{Tag/NONE Tag/HASH_MAP} tag)
                                          (wtypes/->XITDBWriteHashMap (WriteHashMap. cursor))

                                          (= Tag/ARRAY_LIST tag)
                                          (wtypes/->XITDBWriteArrayList (WriteArrayList. cursor)))]
                                    (apply f (concat [obj] args)))))))


(defprotocol IHistory
  (history [this]))

(deftype XITDBDatabase [db]
  IHistory
  (history [this]
    (db-history db))
  clojure.lang.IDeref
  (deref [_]
    (let [history (db-history db)
          cursor (.getCursor history -1)]
      (xtypes/read-from-cursor cursor)))

  clojure.lang.IAtom
  (reset [this new-value]
    (let [history (db-history db)]
      (xitdb-reset! history new-value)
      new-value))
  (swap [this f]
    (xitdb-swap! db f)
    (deref this))

  (swap [this f a]
    (xitdb-swap! db f a)
    (deref this))

  (swap [this f a1 a2]
    (xitdb-swap! db f a1 a2)
    (deref this))

  (swap [this f x y args]
    (apply xitdb-swap! (concat [db f x y] args))
    (deref this)))


(defn xit-db [filename]
  (->XITDBDatabase (open-database filename)))

(defn xitdb-assoc-in! [db k v]
  (let [history (history db)]
    (append-context history (fn [cursor]
                              (util/xitdb-assoc-in cursor k v)))))


