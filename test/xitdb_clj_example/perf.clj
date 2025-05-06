(ns xitdb-clj-example.perf
  (:require
    [clojure.test :refer :all]
    [xitdb-clj-example.xitdb.db :as xdb]
    [xitdb-clj-example.gen-test-data :as gen-test-data]
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


(def test-data {:users [{:id 1
                         :name "Alice"
                         :active 1
                         :settings {:theme "dark"
                                    :notifications 1}}
                        {:id 2
                         :name "Bob"
                         :active 0
                         :settings {:theme "light"
                                    :notifications 0}}]
                :config {:database {:host "localhost"
                                    :port 5432
                                    :credentials {:username "admin"
                                                  :password "secret"}}
                         :features [:reporting :analytics :api]}
                :stats {:visits 12345
                        :by-country {"US" 432
                                     "UK" 123
                                     "DE" 98}
                        :by-browser [{:name "Chrome", :count 8765}
                                     {:name "Firefox", :count 3421}]}})

(deftest MemDbTest
  (let [db (xdb/xit-db :memory)]
    (reset! db (reset! db test-data))
    (is (= test-data (materialize @db)))))

(deftest DatabaseTest
  (testing "Creating the db"
    (let [db (xdb/xit-db "test.xdb")]
      (testing "Resetting to map"
        (reset! db (reset! db test-data)))
      (xdb/close-db! db)))

  (testing "Reading last value"
    (let [db (xdb/xit-db "test.xdb")]
      (is (= test-data (materialize @db)))
      (xdb/close-db! db))))


(def photo-app {:photos
                {;; id → photo map
                 "p-1" {:id        "p-1"
                        :width     4032
                        :height    3024
                        :location  {:lat 41.3931  :lon 2.1615   :place "Barcelona – Parc de la Ciutadella"}
                        :tags      #{"park" "sunset" "friends"}}

                 "p-2" {:id        "p-2"
                        :width     2000
                        :height    3000
                        :location  {:lat 42.6951  :lon 0.0420   :place "Pyrenees – Ordesa Valley"}
                        :tags      #{"hike" "mountain" "waterfall"}}

                 "p-3" {:id        "p-3"
                        :width     1080
                        :height    1920
                        :location  {:lat 41.1189  :lon 1.2445   :place "Tarragona – Roman Amphitheatre"}
                        :tags      #{"history" "ruins"}}

                 "p-4" {:id        "p-4"
                        :width     3024
                        :height    3024
                        :location  {:lat 41.4145  :lon 2.1527   :place "Barcelona – Park Güell"}
                        :tags      #{"gaudi" "tiles" "art"}}

                 "p-5" {:id        "p-5"
                        :width     5120
                        :height    2880
                        :location  {:lat 41.6080  :lon 0.6229   :place "Lleida – Canola Fields"}
                        :tags      #{"yellow" "fields" "spring"}}}

                :albums
                {;; id → album map
                 "a-1" {:id          "a-1"
                        :title       "Weekend in Barcelona"
                        :description "Photos from wandering around BCN—parks, Gaudí, and sunsets."
                        :photo-ids   #{"p-1" "p-4"}}

                 "a-2" {:id          "a-2"
                        :title       "Pyrenees Hiking Trip"
                        :description "Three‑day trek through Ordesa Valley and surrounding peaks."
                        :photo-ids   #{"p-2"}}

                 "a-3" {:id          "a-3"
                        :title       "Catalonia Road‑Trip Highlights"
                        :description "Random gems we found driving across Catalonia."
                        :photo-ids   #{"p-3" "p-5"}}}})


(deftest PhotoAppTest
  (let [db (xdb/xit-db :memory)]
    (reset! db photo-app)
    (materialize @db)))

(deftest TestDataTest
  (let [db (xdb/xit-db :memory)
        test-data (gen-test-data/gen-users 2 {:num-photos 2})]
    (reset! db test-data)
    (is (= "p-u-0-0"
           (get-in @db ["u-0" :photos "p-u-0-0" :id])))
    (is (= "p-u-1-0"
           (get-in @db ["u-1" :photos "p-u-1-0" :id])))))

(defn all-pairs-equal? [tuples]
  (every? (fn [[a b]] (= a b)) tuples))

(defn insert-test-data! [db config]
  (println "Inserting test data " config)
  (time
    (gen-test-data/gen-users-db db config)))

(comment
  (let [db (xdb/xit-db "5-users.xdb")]
    (time
      (println "read-count:" (count @db)))
    (swap! db (fn [root-map]
                (time (println "count:" (count root-map)))))
    (xdb/close-db! db)))

(comment
  (let [db (xdb/xit-db "5-users.xdb")]

    (insert-test-data! db {:num-photos 5
                           :num-records 10000
                           :batch-size 1000
                           :start-from 1000000})
    (xdb/close-db! db)))

(defn test-scenario-2 []
  (let [filename "5-users.xdb"
        num-users 1000000
        num-photos 3
        batch-size 50000
        num-searches 1000000]
    (println "Opening db. " {:num-users num-users :num-photos num-photos :num-searches num-searches})
    (let [db (xdb/xit-db filename)
          _ (println "performing " num-searches " nested lookups")
          results (time
                    (doall
                      (for [i (range num-searches)]
                        (let [user-id (rand-int num-users)
                              username (str "u-" user-id)
                              expected-id (str "p-" username "-0")]
                          [expected-id (get-in @db [username :photos expected-id :id])]))))]
      (is (all-pairs-equal? results)))))

(comment
  (future (test-scenario-2)))