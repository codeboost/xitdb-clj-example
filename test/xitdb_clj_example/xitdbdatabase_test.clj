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

      (swap! db assoc-in [:foo :bar :some] [1 2 3 4])
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
      (swap! db assoc-in [1] 20)
      (is (= [1 20 3 4] (materialize @db)))

      ;; Test adding elements
      (swap! db assoc-in [4] 5)
      (is (= [1 20 3 4 5] (materialize @db))))))

(deftest map-corner-cases-test
  (let [db (xdb/xit-db :memory)]

    (testing "Empty map operations"
      (reset! db {})
      (is (= {} (materialize @db)))
      (swap! db assoc-in [:foo] "bar")
      (is (= {:foo "bar"} (materialize @db))))

    (testing "Nested empty collections"
      (reset! db {:empty-map {} :empty-vec []})
      (is (= {:empty-map {} :empty-vec []} (materialize @db)))
      (swap! db assoc-in [:empty-map :key] "value")
      (is (= {:empty-map {:key "value"} :empty-vec []} (materialize @db))))

    (testing "Special keys"
      (reset! db {})
      (swap! db assoc-in [:ns/keyword] "namespaced")
      (swap! db assoc-in ["string-key"] "string")
      (is (= {:ns/keyword "namespaced" "string-key" "string"} (materialize @db))))

    (testing "Creating nested paths"
      (reset! db {})
      (swap! db assoc-in [:a :b :c :d] "deep")
      (is (= {:a {:b {:c {:d "deep"}}}} (materialize @db))))

    (testing "Replacing data types"
      (reset! db {:foo {:bar "string"}})
      (swap! db assoc-in [:foo :bar] [1 2 3])
      (is (= {:foo {:bar [1 2 3]}} (materialize @db)))
      (swap! db assoc-in [:foo :bar] {:nested "map"})
      (is (= {:foo {:bar {:nested "map"}}} (materialize @db))))

    (testing "Numeric and boolean keys"
      (reset! db {})
      (swap! db assoc-in [1] "numeric")

      (is (= {"1" "numeric"} (materialize @db)) "Keys are stringified")
      (swap! db assoc-in [true] "boolean")
      (is (= {"1" "numeric" "true" "boolean"} (materialize @db))))))



(deftest SwapTest
  (let [db (xdb/xit-db :memory)]
    (reset! db {:foo :bar})
    (is (= {:foo :bar} (materialize @db)))

    (testing "arity-1 assoc"
      (swap! db #(assoc % :some 43))
      (is (= {:foo :bar :some 43} (materialize @db))))

    (testing "arity-2 assoc"
      (swap! db assoc :some 44)
      (is (= {:foo :bar :some 44} (materialize @db))))

    (testing "arity-3 assoc"
      (reset! db {:users {"1" {:name "john"}}})
      (is (= {:users {"1" {:name "john"}}} (materialize @db)))

      (swap! db assoc-in [:users "1" :age] 44)
      (materialize @db)
      (is (= {:users {"1" {:name "john" :age 44}}} (materialize @db))))

    (testing "dissoc"
      (reset! db {:users {"1" {:name "john"}}})
      (swap! db dissoc :users)
      (is (= {} (materialize @db)))

      (reset! db {:users [] :foo :stays})
      (swap! db dissoc :users)
      (is (= {:foo :stays} (materialize @db))))

    (testing "more dissoc"
      (reset! db {:users {"1" {:name "john"}}})
      (swap! db dissoc :users)
      (is (= {} (materialize @db))))))

(deftest SwapArray
  (let [db (xdb/xit-db :memory)]
    (testing "assoc"
      (reset! db [1 2 3])
      (swap! db #(assoc % 0 44))
      (is (= [44 2 3] (materialize @db)))

      (swap! db #(assoc % 3 99))
      (is (= [44 2 3 99] (materialize @db)))
      @db)

    (testing "conj"
      (reset! db [1 2 3])
      (swap! db conj 55)
      (is (= [1 2 3 55] (materialize @db))))

    (testing "assoc-in"
      (reset! db [1 2 {:title "Untitled"} 3 4])
      (swap! db assoc-in [2 :title] "Titled")
      (is (= [1 2 {:title "Titled"} 3 4]
             (materialize @db))))

    (testing "update-in"
      (reset! db [1 2 {:users [{:name "jp"}
                               {:name "cj"}]} 3 4])
      (swap! db update-in [2 :users] conj {:name "fl"})
      (is (= [1 2 {:users [{:name "jp"} {:name "cj"} {:name "fl"}]} 3 4]
             (materialize @db))))))


(deftest AssocInTest
  (let [db (xdb/xit-db :memory)]
    (testing "assoc-in more"
      (reset! db [1 2 {:users [{:name "jp"}
                               {:name "cj"}]} 3 4])
      (swap! db assoc-in [2 :users 1 :name] "maria")
      (is (= [1 2 {:users [{:name "jp"} {:name "maria"}]} 3 4]
             (materialize @db)))
      @db)))

#_(deftest SwapMerge
    (let [db (xdb/xit-db :memory)]
      (reset! db {"1" {:name "jp"}
                  "2" {:name "cj"}})

      (swap! db merge {"3" {:name "maria"}})
      (materialize @db)))



