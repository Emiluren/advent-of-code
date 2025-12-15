(require '[clojure.string :as str])

(defn process-input [input-str]
  (->> (for [l (str/split-lines input-str)]
         (let [[device-str outputs-str] (str/split l #": ")]
           {(keyword device-str) (->> (str/split outputs-str #" ")
                                      (mapv keyword))}))
       (reduce merge)))

(def input-map (process-input (slurp "11input")))

(defmacro defn-memo [name args & body]
  `(def ~name
     (memoize (fn ~args
                ~@body))))

(defn-memo get-paths [from to]
  (if (= to from)
    [[to]]
    (for [output (get input-map from)
          sub-path (get-paths output to)]
      (lazy-seq (cons from sub-path)))))

(println "Part 1:"
         (count (get-paths :you :out)))

(defn-memo count-paths [from to]
  (if (= from to)
    1
    (->> (for [output (get input-map from)]
           (count-paths output to))
         (reduce +))))

(println "Part 2:"
         (+ (* (count-paths :svr :fft)
               (count-paths :fft :dac)
               (count-paths :dac :out))
            (* (count-paths :svr :dac)
               (count-paths :dac :fft)
               (count-paths :fft :out))))
