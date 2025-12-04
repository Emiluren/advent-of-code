(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn process-input [input-file]
  (->> (str/split-lines (slurp input-file))
       (keep-indexed (fn [row-idx row]
                       (keep-indexed (fn [col-idx c]
                                       (if (= c \@)
                                         [row-idx col-idx]))
                                     row)))
       (map set)
       (apply set/union)))

(def input-lines (process-input "4input"))

(defn get-adjacent [r c grid]
  (->> (for [r-offset (range -1 2)
             c-offset (range -1 2)
             :when (not (= 0 r-offset c-offset))]
         [r-offset c-offset])
       (keep (fn [[r-offset c-offset]]
               (get grid [(+ r r-offset) (+ c c-offset)])))))

(defn valid-coords [grid]
  (for [[r c] grid
        :when (< (count (get-adjacent r c grid)) 4)]
    [r c]))

(println "Part 1:"
         (count (valid-coords input-lines)))

(defn iter-remove-valid [grid]
  (loop [grid grid
         total-count 0]
    (let [coords-to-remove (valid-coords grid)
          remove-count (count coords-to-remove)]
      (if (= 0 remove-count)
        total-count
        (recur (set/difference grid coords-to-remove)
               (+ total-count remove-count))))))

(println "Part 2:"
         (iter-remove-valid input-lines))
