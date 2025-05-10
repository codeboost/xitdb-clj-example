(ns xitdb.common
  (:require
    [xitdb.xitdb-util :as util]))


(defprotocol ISlot
  (-slot [this]))

(defprotocol IReadFromCursor
  (-read-from-cursor [this]))

(defprotocol IMaterialize
  (-materialize [this]))



(defn array-seq
  [ral]
  "The cursors used must implement the IReadFromCursor protocol."
  (util/array-seq ral #(-read-from-cursor %)))

(defn map-seq
  [rhm]
  "The cursors used must implement the IReadFromCursor protocol."
  (util/map-seq rhm #(-read-from-cursor %)))

(defn materialize [v]
  (if (satisfies? IMaterialize v)
    (-materialize v)
    v))