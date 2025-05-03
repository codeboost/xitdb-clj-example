(ns xitdb-clj-example.xitdbdatabase-test
  (:require
    [clojure.test :refer :all]
    [xitdb-clj-example.xitdb.db :as xdb]
    [xitdb-clj-example.xitdb-types :as types])
  (:import (xitdb_clj_example.xitdb_types XITDBArrayList XITDBHashMap)))

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

(deftest DatabaseTest
  (let [db (xdb/xit-db :memory)]
    (testing "Resetting to map"
      (reset! db {:foo :bar})
      (is (= {:foo :bar} (materialize @db)))

      (reset! db {:foo {:bar :Baz}})
      (is (= {:foo {:bar :Baz}} (materialize @db)))

      (reset! db {:foo {:bar {:some "baz"}}})

      (is (= {:foo {:bar {:some "baz"}}} (materialize @db)))

      (xdb/xitdb-assoc-in! db [:foo :bar :some] [1 2 3 4])
      (is (= {:foo {:bar {:some [1 2 3 4]}}} (materialize @db))))))

(deftest array-reset-test
  (let [db (xdb/xit-db :memory)]
    (testing "Resetting to array"
      ;; Start with a fresh database and reset to vector
      (reset! db [1 2 3 4])
      (is (= [1 2 3 4] (materialize @db)))

      ;; Test accessing elements
      (is (= 3 (get @db 2)))

      ;; Test updating an element
      (xdb/xitdb-assoc-in! db [1] 20)
      (is (= [1 20 3 4] (materialize @db)))

      ;; Test adding elements
      (xdb/xitdb-assoc-in! db [4] 5)
      (is (= [1 20 3 4 5] (materialize @db))))))

(deftest map-corner-cases-test
  (let [db (xdb/xit-db :memory)]

    (testing "Empty map operations"
      (reset! db {})
      (is (= {} (materialize @db)))
      (xdb/xitdb-assoc-in! db [:foo] "bar")
      (is (= {:foo "bar"} (materialize @db))))


    (testing "Nested empty collections"
      (reset! db {:empty-map {} :empty-vec []})
      (is (= {:empty-map {} :empty-vec []} (materialize @db)))
      (xdb/xitdb-assoc-in! db [:empty-map :key] "value")
      (is (= {:empty-map {:key "value"} :empty-vec []} (materialize @db))))


    (testing "Special keys"
      (reset! db {})
      (xdb/xitdb-assoc-in! db [:ns/keyword] "namespaced")
      (xdb/xitdb-assoc-in! db ["string-key"] "string")
      (is (= {:ns/keyword "namespaced" "string-key" "string"} (materialize @db))))

    (testing "Creating nested paths"
      (reset! db {})
      (xdb/xitdb-assoc-in! db [:a :b :c :d] "deep")
      (is (= {:a {:b {:c {:d "deep"}}}} (materialize @db))))

    (testing "Replacing data types"
      (reset! db {:foo {:bar "string"}})
      (xdb/xitdb-assoc-in! db [:foo :bar] [1 2 3])
      (is (= {:foo {:bar [1 2 3]}} (materialize @db)))
      (xdb/xitdb-assoc-in! db [:foo :bar] {:nested "map"})
      (is (= {:foo {:bar {:nested "map"}}} (materialize @db))))

    (testing "Numeric and boolean keys"
      (reset! db {})
      (xdb/xitdb-assoc-in! db [1] "numeric")

      (is (= {"1" "numeric"} (materialize @db)) "Keys are stringified")
      (xdb/xitdb-assoc-in! db [true] "boolean")
      (is (= {"1" "numeric" "true" "boolean"} (materialize @db))))))



(deftest SwapTest
  (let [db (xdb/xit-db :memory)]
    (reset! db {:foo :bar})
    (swap! db (fn [cursor]))

    @db))
