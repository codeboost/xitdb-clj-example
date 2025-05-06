(ns xitdb-clj-example.gen-test-data
  (:require
    [clojure.string :as str]
    [xitdb-clj-example.xitdb.db :as xdb]))

;; ── some toy vocab for tags ───────────────────────────────────────────────────
(def tag-pool ["sunset" "park" "friends" "hike" "mountain" "city" "night"
               "portrait" "ruins" "art" "snow" "beach" "forest" "flower"])

(defn rand-tags []
  ;; sets a vec for now.
  (->> tag-pool shuffle (take (inc (rand-int 5))) set vec))

(defn rand-location []
  {:lat (- 90  (rand 180))   ;; quick ‘n dirty – any point on earth
   :lon (- 180 (rand 360))})

;; ── photo / album builders ────────────────────────────────────────────────────
(defn gen-photo
  "Build a single photo map for user‑id, seq‑num."
  [user-id n]
  (let [pid (format "p-%s-%d" user-id n)]
    {:id          pid
     :width       (+ 600 (rand-int 4400))
     :height      (+ 600 (rand-int 4400))
     :location    (rand-location)
     :tags        (rand-tags)
     :title       (format "User %s Photo %d" user-id n)
     :description (format "user %s photo %d description" user-id n)}))

(defn gen-album
  "Album picks a random subset of the user’s photo‑ids."
  [user-id n photo-ids]
  (let [aid    (format "a-%s-%d" user-id n)
        picked (->> photo-ids shuffle (take (inc (rand-int (count photo-ids)))) set vec)]
    {:id          aid
     :title       (format "User %s Album %d" user-id n)
     :description (format "random album %d for user %s" n user-id)
     :photo-ids   picked}))

;; ── user builder ──────────────────────────────────────────────────────────────
(defn gen-user
  "Return one user with an inner :photos and :albums map."
  [user-idx {:keys [max-photos num-photos]
             :or {max-photos 30}}]
  (let [uid         (format "u-%d" user-idx)
        n-photos    (or num-photos (inc (rand-int max-photos)))
        photo-ids   (map #(format "p-%s-%d" uid %) (range n-photos))
        photos      (into {} (map (fn [i] [(nth photo-ids i)
                                           (gen-photo uid i)])
                                  (range n-photos)))
        n-albums    (inc (rand-int 6))          ; 1‑6 albums
        albums      (into {} (map (fn [i]
                                    (let [al (gen-album uid i photo-ids)]
                                      [(:id al) al]))
                                  (range n-albums)))]
    {:id uid
     :photos photos
     :albums albums}))

(defn gen-users [ids config]
  (map (fn [uid]
         (gen-user uid config)) ids))

(comment
  ;; see it in the REPL:
  (gen-users [1 2 3] {:num-photos 2}))

(defn gen-users-db
  [db {:keys [num-records batch-size start-from]
       :or {batch-size 1000
            start-from 0} :as config}]
  (println "Generating test data for" num-records "records. config:" config)
  (let [batch-size (min num-records batch-size)
        num-batches (/ num-records batch-size)]
    (dotimes [batch-id num-batches]
      (println "Generating batch " batch-id "of" num-batches)
      (let [start (+ start-from (* batch-size batch-id))
            uids (take batch-size (range start))
            users (gen-users uids config)
            kvs (mapcat (fn [user]
                          [(:id user) user]) users)]
        (time
          (swap! db (fn [m]
                      (apply assoc (concat [m] kvs)))))))))

(comment
  (let [db (xdb/xit-db :memory)]
    (gen-users-db db 20 {:num-photos 1})
    @db))
