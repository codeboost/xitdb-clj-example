(ns xitdb-clj-example.xitdb
  (:require
    [xitdb-clj-example.xitdb.db :as xdb :refer []]
    [xitdb-clj-example.xitdb-types :as xtypes]
    [xitdb-clj-example.xitdb-util :as util]))

(defn example-3 []
  (let [db (xdb/xit-db :memory)]
    (reset! db {:users {"1" {:items {:foo :bar}}}})
    (xdb/xitdb-assoc-in! db [:users "1" :items :foo] "VALUE")
    @db))

(defn example-32 []
  (let [db (xdb/xit-db :memory)]
    (reset! db {:users {"1" {:name "Florin"}}})
    (xdb/xitdb-assoc-in! db [:users "1" :name] "FLORIN")
    @db))

(defn example-2 []
  (let [db (xdb/xit-db :memory)]
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
  (let [db (xdb/xit-db :memory)
        history (xdb/history db)]
    (xdb/xitdb-reset! history {:type :pc :memory 32
                           :details {:title "something"}
                           :my-array ["one" "two" "three"]})
    (let [m (xdb/xitdb-read history)]
      (vec (get m ":my-array")))))

