(require '[clojure.string :as str])

(defn pad-input [lines]
  (let [line-len (+ 2 (count (get lines 0)))
        empty-line (vec (repeat line-len \.))
        padded-lines (for [l lines]
                       (vec (str \. l \.)))]
    (vec (cons empty-line (conj (vec padded-lines) empty-line)))))

(defn process-input [input-file]
  (pad-input (str/split-lines (slurp input-file))))

(def input-lines (process-input "4input"))

(defn get-adjacent [r c grid]
  (for [r-offset (range -1 2)
        c-offset (range -1 2)
        :when (not (= 0 r-offset c-offset))]
    (get-in grid [(+ r r-offset) (+ c c-offset)])))

(defn valid-coords [grid]
  (for [r (range 1 (- (count grid) 1))
        c (range 1 (- (count (get grid 0)) 1))
        :let [adjacent-count (get (frequencies (get-adjacent r c grid)) \@ 0)]
        :when (and
               (= \@ (get-in grid [r c]))
               (< adjacent-count 4))]
    [r c]))

(println "Part 1:"
         (count (valid-coords input-lines)))

(defn remove-coords [grid coords]
  (loop [grid grid
         coords coords]
    (if (seq coords)
      (recur (assoc-in grid (first coords) \.)
             (rest coords))
      grid)))

(defn iter-remove-valid [grid]
  (loop [grid grid
         total-count 0]
    (let [coords-to-remove (valid-coords grid)
          remove-count (count coords-to-remove)]
      (if (= 0 remove-count)
        total-count
        (recur (remove-coords grid coords-to-remove)
               (+ total-count remove-count))))))

(println "Part 2:"
         (iter-remove-valid (process-input "4input")))
