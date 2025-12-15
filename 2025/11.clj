(require '[clojure.string :as str])

(defn process-input [input-str]
  (->> (for [l (str/split-lines input-str)]
         (let [[device-str outputs-str] (str/split l #": ")]
           {(keyword device-str) (->> (str/split outputs-str #" ")
                                      (mapv keyword))}))
       (reduce merge)))

(def input-map (process-input (slurp "10input")))

(defmacro defn-memo [name args & body]
  `(def ~name
     (memoize (fn ~args
                ~@body))))

(defn get-paths [from]
  (if (= :out from)
    [[:out]]
    (for [output (get input-map from)
          sub-path (get-paths output)]
      (cons from sub-path))))

(println "Part 1:"
         (count (get-paths :you)))

(defn count-paths [from]
  (if (= :out from)
    1
    (for [output (get input-map from)
          sub-path (get-paths output)]
      (cons from sub-path))))

;;(def paths2 (get-paths :svr))
