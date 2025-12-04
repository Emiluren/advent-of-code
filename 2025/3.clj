(require '[clojure.string :as str])

(def test-input
  "987654321111111\n811111111111119\n234234234234278\n818181911112111")

(defn calc-joltage [s]
  (parse-long (apply str s)))

(def find-max-joltage
  (memoize (fn [s len]
             (cond
               (= len 0) ""
               (= (count s) len) (calc-joltage s)
               :else (max (find-max-joltage (rest s) len)
                          (calc-joltage (str (first s)
                                             (find-max-joltage (rest s) (- len 1)))))))))

(defn calc-max-joltage-sum [input len]
  (->> (str/split-lines input)
       (map #(find-max-joltage % len))
       (reduce +)))

(println "Part 1: "
         (calc-max-joltage-sum (slurp "3input") 2))

(println "Part 2: "
         (calc-max-joltage-sum (slurp "3input") 12))
