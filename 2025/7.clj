(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn process-input [input-str]
  (for [l (str/split-lines input-str)]
    (set (keep-indexed #(if (not (= \. %2)) %1) l))))

(defn split-beams [beams-to-split]
  (reduce set/union
          (map (fn [b] #{(- b 1) (+ b 1)})
               beams-to-split)))

(println "Part 1:"
         (let [splitter-lines (process-input (slurp "7input"))
               beam-states (reductions (fn [beams splitters]
                                         (let [beams-to-split (set/intersection beams splitters)]
                                           (set/union (set/difference beams beams-to-split)
                                                      (split-beams beams-to-split))))
                                       splitter-lines)]
           (->> (map (fn [beams splitters]
                       (count (set/intersection beams splitters)))
                     (butlast beam-states)
                     (rest splitter-lines))
                (reduce +))))
