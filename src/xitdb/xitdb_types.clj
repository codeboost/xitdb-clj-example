(ns xitdb.xitdb-types
  (:require
    [xitdb.array-list :as xarray-list]
    [xitdb.common :as common]
    [xitdb.hash-map :as xhash-map]
    [xitdb.linked-list :as xlinked-list]
    [xitdb.xitdb-util :as util])
  (:import
    [io.github.radarroark.xitdb ReadArrayList ReadCursor ReadHashMap Slot Tag WriteArrayList WriteCursor WriteHashMap]
    (xitdb.array_list XITDBArrayList)
    (xitdb.hash_map XITDBHashMap)))


(defn read-from-cursor [^ReadCursor cursor for-writing?]
  (let [value-tag (some-> cursor .slot .tag)]
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
      (if for-writing?
        (xhash-map/xwrite-hash-map cursor)
        (xhash-map/xhash-map cursor))

      (= value-tag Tag/ARRAY_LIST)
      (if for-writing?
        (xarray-list/xwrite-array-list cursor)
        (xarray-list/xarray-list cursor))

      :else
      nil)))

(extend-protocol common/IReadFromCursor
  ReadCursor
  (-read-from-cursor [this]
    (read-from-cursor this false))

  WriteCursor
  (-read-from-cursor [this]
    (read-from-cursor this true)))

(defn ^Slot slot-for-value! [^WriteCursor cursor v]
  (cond
    (satisfies? common/ISlot v)
    (common/-slot v)
    :else
    (util/v->slot! cursor v)))

(defn materialize
  "Converts a xitdb data structure `v` to a clojure data structure.
  This has the effect of reading the whole data structure into memory."
  [v]
  (cond
    (instance? XITDBArrayList v)
    (reduce (fn [a v]
              (conj a (materialize v))) [] (seq v))

    (instance? XITDBHashMap v)
    (reduce (fn [m [k v]]
              (assoc m k (materialize v))) {} (seq v))
    :else
    v))