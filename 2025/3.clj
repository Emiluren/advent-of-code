(require '[clojure.string :as str])

(def *test-input*
  ["987654321111111"
   "811111111111119"
   "234234234234278"
   "818181911112111"])

#_(def s (*test-input* 0))

(defn find-max-joltage [s]
  (apply max
         (for [i (range (count s))
               j (range (+ i 1) (count s))]
           (parse-long (str (get s i) (get s j))))))

(println "Part 1: "
         (->> (str/split-lines (slurp "3input"))
          (map find-max-joltage)
          (reduce +)))
