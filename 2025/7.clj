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

(def test-beams (simulate-beam-states (process-input (slurp "7testinput"))))

; 10434635933 too low
; 25 instead of 40 for testinput
(println "Part 2:"
         (reduce + (vals (last (simulate-beam-states (process-input (slurp "7testinput")))))))

(defn cols-between [line]
  (let [sorted-line-keys (sort (keys line))]
    (cons (first sorted-line-keys)
          (map - (rest sorted-line-keys) (butlast sorted-line-keys)))))

(defn beam-line-to-seq [line]
  (mapcat (fn [col-n v] (conj (vec (repeat col-n 0)) v))
          (cols-between line)
          (vals line)))

(defn beam-line-str [line col-width]
  (->> (for [c (beam-line-to-seq line)]
         (if (= 0 c)
           (apply str (repeat col-width " "))
           (format (str \% col-width \d) c)))
       (apply str)))

(defn print-states [beam-states]
  (let [max-v (apply max (vals (last beam-states)))
        col-width (+ 1 (count (str max-v)))]
   (doseq [l beam-states]
     (println (beam-line-str l col-width)))))
