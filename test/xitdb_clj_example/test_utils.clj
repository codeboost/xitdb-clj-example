(ns xitdb-clj-example.test-utils
  (:require
    [clojure.test :refer :all]
    [xitdb.db :as xdb]
    [xitdb.xitdb-types :as types])
  (:import (xitdb.xitdb_types XITDBArrayList XITDBHashMap)))


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

(defprotocol DbEqualToAtom
  (db-equal-to-atom? [this]))

(deftype DBWithAtom [db test-atom]
  DbEqualToAtom
  (db-equal-to-atom? [this]
    (= (materialize @db) @test-atom))

  xdb/ICloseDB
  (close-db! [this]
    (xdb/close-db! db))

  clojure.lang.IDeref
  (deref [_]
    (materialize
      (deref db)))

  clojure.lang.IAtom
  (reset [this new-value]
    (reset! test-atom new-value)
    (reset! db new-value))
  (swap [this f]
    (swap! test-atom f)
    (swap! db f))

  (swap [this f a]
    (swap! test-atom f a)
    (swap! db f a))

  (swap [this f a1 a2]
    (swap! test-atom f a1 a2)
    (swap! db f a1 a2))

  (swap [this f x y args]
    (apply swap! (concat [test-atom f x y] args))
    (apply swap! (concat [db f x y] args))))

(defn instrumented-db [db]
  (let [a (atom nil)]
    (->DBWithAtom db a)))

(def test-source :memory) ;; :memory or filename

(defn test-db []
  (instrumented-db (xdb/xit-db test-source)))

(defn test-memory-db-raw []
  (xdb/xit-db :memory))

(defn test-memory-db-a []
  (instrumented-db (atom nil)))

(defmacro with-db
  "Execute body with a database connection, then ensure database is closed.

  Usage:
  (with-db [db (xdb/open-db \"some.xdb\")]
    ... code using db ...)"
  [binding & body]
  (let [db-name (first binding)
        db-expr (second binding)]
    `(let [~db-name ~db-expr]
       (try
         ~@body
         (finally
           (xdb/close-db! ~db-name))))))