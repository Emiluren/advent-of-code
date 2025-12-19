(require '[clojure.string :as str])

(defn process-region [region-str]
  (let [[size quantities] (str/split region-str #": ")]
    {:size (mapv parse-long (str/split size #"x"))
     :quantities (mapv parse-long (str/split quantities #" "))}))

(defn process-input [input-str]
  (let [sections (str/split input-str #"\n\n")
        shapes (for [s (butlast sections)]
                 (vec (rest (str/split-lines s))))
        regions (str/split-lines (last sections))]
    {:shapes shapes
     :regions (mapv process-region regions)}))

(defn flip-shape [shape]
  (map reverse shape))

(defn rotate-shape [shape]
  (flip-shape (apply map vector shape)))

(defn prn-shape [shape]
  (doseq [l shape]
    (doseq [c l]
      (print c))
    (println)))

(defn maybe-place [shape region]
  )

(println)
(prn-shape (rotate-shape (rotate-shape (first (:shapes (process-input (slurp "12testinput")))))))
