(ns xitdb-clj-example.xitdb
  (:require
    [xitdb-clj-example.xitdb-types :as xtypes]
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



(defn xitdb-reset! [history new-value]
  (append-context
    history
    (fn [cursor]
      (v->xitdb cursor new-value))))


(defn xitdb-read [history]
  (let [cursor (.getCursor history -1)]
    (xtypes/read-from-cursor cursor)))

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
      new-value)))

(defn xitdb-assoc-in! [db k v]
  (let [history (history db)]
    (append-context history (fn [cursor]
                              (let [wm (WriteHashMap. cursor)]
                                (util/xitdb-assoc-in wm k v))))))

(defn example-3 []
  (let [db (->XITDBDatabase (open-db :memory))]
    (reset! db {:users {1 {:name "Florin"}}})
    (xitdb-assoc-in! db [:users 1] {:name "FLORIN"})
    @db))

(defn example-2 []
  (let [db (->XITDBDatabase (open-db :memory))]
    (reset! db {:users  [{:id          1
                          :name        "Alice Smith"
                          :age         32
                          :active      true
                          :roles       ["admin" "developer"]
                          :preferences {:theme         "dark"
                                        :notifications true
                                        :dashboard     {:widgets ["calendar" "tasks" "stats"]
                                                        :layout  "grid"}}
                          :metrics     {:logins      27
                                        :last-active 1623451289}}
                         {:id          2
                          :name        "Bob Johnson"
                          :age         45
                          :active      true
                          :roles       ["user"]
                          :preferences {:theme         "light"
                                        :notifications false
                                        :dashboard     {:widgets ["calendar"]
                                                        :layout  "list"}}
                          :metrics     {:logins      14
                                        :last-active 1623442122}}]
                :config {:version  "1.0.3"
                         :features ["search" "export" "sharing"]
                         :limits   {:max-users   100
                                    :max-storage 5000}}
                :stats  {:total-users  2
                         :active-users 2}})
    @db))

(defn example-1 []
  (let [db (open-db :memory)
        history (db-history db)]
    (xitdb-reset! history {:type :pc :memory 32
                           :details {:title "something"}
                           :my-array ["one" "two" "three"]})
    (let [m (xitdb-read history)]
      (vec (get m ":my-array")))))

