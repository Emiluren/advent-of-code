(require '[clojure.string :as str])

(def test-input
  "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defn process-input [input-str]
  (->> (for [l (str/split-lines input-str)]
         (str/split (str/trim l) #"\s+"))
       (apply map vector)
       (map reverse)
       (map #(cons ({"*" *, "+" +} (first %))
                   (map parse-long (rest %))))))

(println "Part 1:"
         (->> (process-input (slurp "6input"))
              (map #(apply (first %) (rest %)))
              (reduce +)))
