(ns main)

(def input-data (clojure.edn/read-string (slurp "processed-input.edn")))

(defn find-in-same-group [start-id db]
  (loop [to-search [start-id]
         visited #{start-id}]
    (if (seq to-search)
      (let [current-id (first to-search)
            local-neighbours (db current-id)]
        (recur (apply conj (rest to-search)
                      (filter (complement visited) local-neighbours))
               (apply conj visited local-neighbours)))
      visited)))

(defn solve-part-1 []
  (count (find-in-same-group 0 input-data)))

(defn number-of-groups [db]
  (loop [not-in-groups (range 0 2000)
         number-of-groups 0]
    (if (seq not-in-groups)
      (recur (filter (complement (find-in-same-group
                                  (first not-in-groups)
                                  db))
                     not-in-groups)
             (inc number-of-groups))
      number-of-groups)))

(defn solve-part-2 []
  (number-of-groups input-data))

(defn -main
  [& args]
  (println "Hello world!"))
