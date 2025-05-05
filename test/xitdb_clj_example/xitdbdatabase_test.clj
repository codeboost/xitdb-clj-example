(ns xitdb-clj-example.xitdbdatabase-test
  (:require
    [clojure.test :refer :all]
    [xitdb-clj-example.test-utils :as tu]))

(deftest DatabaseTest
  (let [db (tu/test-memory-db)]
    (testing "Resetting to map"
      (reset! db {:foo :bar})
      (is (= {:foo :bar} @db))

      (reset! db {:foo {:bar :Baz}})
      (is (= {:foo {:bar :Baz}} @db))

      (reset! db {:foo {:bar {:some "baz"}}})
      (is (= {:foo {:bar {:some "baz"}}} @db))

      (swap! db assoc-in [:foo :bar :some] [1 2 3 4])
      (is (= {:foo {:bar {:some [1 2 3 4]}}} @db))

      (is (tu/db-equal-to-atom? db)))))

(deftest array-reset-test
  (let [db (tu/test-memory-db)]
    (testing "Resetting to array"
      ;; Start with a fresh database and reset to vector
      (reset! db [1 2 3 4])
      (is (= [1 2 3 4] @db))

      ;; Test accessing elements
      (is (= 3 (get @db 2)))

      ;; Test updating an element
      (swap! db assoc-in [1] 20)
      (is (= [1 20 3 4] @db))

      ;; Test adding elements
      (swap! db assoc-in [4] 5)
      (is (= [1 20 3 4 5] @db))
      (is (tu/db-equal-to-atom? db)))))

(deftest map-corner-cases-test
  (let [db (tu/test-memory-db)]

    (testing "Empty map operations"
      (reset! db {})
      (is (= {} @db))
      (swap! db assoc-in [:foo] "bar")
      (is (= {:foo "bar"} @db)))

    (testing "Nested empty collections"
      (reset! db {:empty-map {} :empty-vec []})
      (is (= {:empty-map {} :empty-vec []} @db))
      (swap! db assoc-in [:empty-map :key] "value")
      (is (= {:empty-map {:key "value"} :empty-vec []} @db)))

    (testing "Special keys"
      (reset! db {})
      (swap! db assoc-in [:ns/keyword] "namespaced")
      (swap! db assoc-in ["string-key"] "string")
      (is (= {:ns/keyword "namespaced" "string-key" "string"} @db)))

    (testing "Creating nested paths"
      (reset! db {})
      (swap! db assoc-in [:a :b :c :d] "deep")
      (is (= {:a {:b {:c {:d "deep"}}}} @db)))

    (testing "Replacing data types"
      (reset! db {:foo {:bar "string"}})
      (swap! db assoc-in [:foo :bar] [1 2 3])
      (is (= {:foo {:bar [1 2 3]}} @db))
      (swap! db assoc-in [:foo :bar] {:nested "map"})
      (is (= {:foo {:bar {:nested "map"}}} @db)))

    (testing "Numeric and boolean keys"
      (reset! db {})
      (swap! db assoc-in [1] "numeric")

      (is (= {"1" "numeric"} @db) "Keys are stringified")
      (swap! db assoc-in [true] "boolean")
      (is (= {"1" "numeric" "true" "boolean"} @db)))

    ;;This fails because keys are strings
    #_(is (tu/db-equal-to-atom? db))))

(deftest SwapTest
  (let [db (tu/test-memory-db)]
    (reset! db {:foo :bar})
    (is (= {:foo :bar} @db))

    (testing "arity-1 assoc"
      (swap! db #(assoc % :some 43))
      (is (= {:foo :bar :some 43} @db)))

    (testing "arity-2 assoc"
      (swap! db assoc :some 44)
      (is (= {:foo :bar :some 44} @db)))

    (testing "arity-3 assoc"
      (reset! db {:users {"1" {:name "john"}}})
      (is (= {:users {"1" {:name "john"}}} @db))

      (swap! db assoc-in [:users "1" :age] 44)
      (is (= {:users {"1" {:name "john" :age 44}}} @db)))

    (testing "dissoc"
      (reset! db {:users {"1" {:name "john"}}})
      (swap! db dissoc :users)
      (is (= {} @db))

      (reset! db {:users [] :foo :stays})
      (swap! db dissoc :users)
      (is (= {:foo :stays} @db)))

    (testing "more dissoc"
      (reset! db {:users {"1" {:name "john"}}})
      (swap! db dissoc :users)
      (is (= {} @db)))
    (is (tu/db-equal-to-atom? db))))

(deftest SwapArray
  (let [db (tu/test-memory-db)]
    (testing "assoc"
      (reset! db [1 2 3])
      (swap! db #(assoc % 0 44))
      (is (= [44 2 3] @db))

      (swap! db #(assoc % 3 99))
      (is (= [44 2 3 99] @db)))

    (testing "conj"
      (reset! db [1 2 3])
      (swap! db conj 55)
      (is (= [1 2 3 55] @db)))

    (testing "assoc-in"
      (reset! db [1 2 {:title "Untitled"} 3 4])
      (swap! db assoc-in [2 :title] "Titled")
      (is (= [1 2 {:title "Titled"} 3 4] @db)))

    (testing "update-in"
      (reset! db [1 2 {:users [{:name "jp"}
                               {:name "cj"}]} 3 4])
      (swap! db update-in [2 :users] conj {:name "fl"})
      (is (= [1 2 {:users [{:name "jp"} {:name "cj"} {:name "fl"}]} 3 4]
             @db)))

    (testing "update-in array"
      (reset! db [1 2 [{:name "jp"} {:name "cj"}] 3 4])
      (swap! db update-in [2 1 :name ] str "-foo")
      (is (= [1 2 [{:name "jp"} {:name "cj-foo"}] 3 4] @db))
      (swap! db update-in [2 1] dissoc :name)
      (is (= [1 2 [{:name "jp"} {}] 3 4] @db))
      (swap! db update-in [2] butlast)
      (is (= [1 2 [{:name "jp"}] 3 4] @db)))

    (is (tu/db-equal-to-atom? db))))

(deftest AssocInTest
  (let [db (tu/test-memory-db)]
    (testing "assoc-in more"
      (reset! db [1 2 {:users [{:name "jp"}
                               {:name "cj"}]} 3 4])
      (swap! db assoc-in [2 :users 1 :name] "maria")
      (is (= [1 2 {:users [{:name "jp"} {:name "maria"}]} 3 4]
             @db))
      (is (tu/db-equal-to-atom? db)))))

(deftest MergeTest
  (let [db (tu/test-memory-db)]
    (reset! db {"1" {:name "jp"}
                "2" {:name "cj"}})

    (swap! db merge {"3" {:name "maria"}})

    (is (= {"1" {:name "jp"}
            "2" {:name "cj"}
            "3" {:name "maria"}}
           @db))

    (swap! db merge {:foo [:bar]})
    (is (= {"1" {:name "jp"}
            "2" {:name "cj"}
            "3" {:name "maria"}
            :foo [:bar]}
           @db))
    (is (tu/db-equal-to-atom? db))))

(deftest IntoTest
  (let [db (tu/test-memory-db)]
    (reset! db {"1" {:name "jp"} "2" {:name "cj"}})
    (swap! db into [[:foo :bar]])
    (is (= {"1" {:name "jp"} "2" {:name "cj"} :foo :bar} @db))

    (reset! db [])
    (swap! db into {:one :two})
    (is (= [[:one :two]] @db))
    (is (tu/db-equal-to-atom? db))))