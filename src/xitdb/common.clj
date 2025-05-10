(ns xitdb.common
  (:require
    [xitdb.xitdb-util :as util]))

(defprotocol ISlot
  (-slot [this]))

(defprotocol IReadFromCursor
  (-read-from-cursor [this]))

(defprotocol IMaterialize
  (-materialize [this]))


(defn materialize [v]
  (if (satisfies? IMaterialize v)
    (-materialize v)
    v))