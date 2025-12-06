(require '[clojure.string :as str])

(def test-input
  "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defn process-input-p1 [input-str]
  (->> (for [l (str/split-lines input-str)]
         (str/split (str/trim l) #"\s+"))
       (apply map vector)
       (map reverse)
       (map #(cons ({"*" *, "+" +} (first %))
                   (map parse-long (rest %))))))

(println "Part 1:"
         (->> (process-input-p1 (slurp "6input"))
              (map #(apply (first %) (rest %)))
              (reduce +)))

(defn process-input-p2 [input-str]
  (let [cols (->> (apply map str (str/split-lines input-str))
                  (partition-by #(re-matches #"\s+" %))
                  (filter #(not (and (= 1 (count %))
                                     (re-matches #"\s+" (first %))))))]
    (for [c cols
          :let [op (last (first c))
                first-num (apply str (butlast (first c)))
                num-strs (cons first-num (rest c))]]
      (cons op (map #(parse-long (str/trim %)) num-strs)))))

(println "Part 2:"
         (->> (process-input-p2 (slurp "6input"))
              (map #(apply ({\* *, \+ +} (first %)) (rest %)))
              (reduce +)))
