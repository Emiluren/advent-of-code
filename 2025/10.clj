(require '[clojure.string :as str])

(defn process-input [input-str]
  (->> (for [l (str/split-lines input-str)]
         (let [[device-str outputs-str] (str/split l #": ")]
           {(keyword device-str) (->> (str/split outputs-str #" ")
                                      (mapv keyword))}))
       (reduce merge)))

(defn get-paths [input-map from]
  (if (= :out from)
    [[:out]]
    (for [output (get input-map from)
          sub-path (get-paths input-map output)]
      (cons from sub-path))))

(println "Part 1:"
         (count (get-paths (process-input (slurp "10input")) :you)))
