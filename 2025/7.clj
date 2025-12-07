(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn process-input [input-str]
  (for [l (str/split-lines input-str)]
    (set (keep-indexed #(if (not (= \. %2)) %1) l))))

(defn split-beams [beams-to-split]
  (apply merge-with +
         (map (fn [[index amount]] {(- index 1) amount
                                    (+ index 1) amount})
              beams-to-split)))

(defn simulate-beam-states [splitter-lines]
  (reductions (fn [beams splitters]
                (let [beams-to-split (select-keys beams splitters)]
                  (merge (apply dissoc beams splitters)
                         (split-beams beams-to-split))))
              {(first (first splitter-lines)) 1}
              (rest splitter-lines)))

(defn part1 [splitter-lines beam-states]
  (->> (map (fn [beams splitters]
                     (count (select-keys beams splitters)))
                   (butlast beam-states)
                   (rest splitter-lines))
              (reduce +)))

(def splitter-lines (process-input (slurp "7input")))
(def beam-states (simulate-beam-states splitter-lines))

(println "Part 1:"
         (part1 splitter-lines beam-states))

; 10434635933 too low
; 25 instead of 40 for testinput
(println "Part 2:"
         (reduce + (vals (last (simulate-beam-states (process-input (slurp "7testinput")))))))
