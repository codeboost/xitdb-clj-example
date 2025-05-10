(ns xitdb.common)


(defprotocol ISlot
  (-slot [this]))

(defprotocol IReadFromCursor
  (-read-from-cursor [this]))