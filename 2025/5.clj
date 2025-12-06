(require '[clojure.string :as str])

(def test-input
  "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defn process-input [input-str]
  (let [[ranges-str ids-str] (str/split input-str #"\n\n")
        ranges (for [l (str/split-lines ranges-str)
                     :let [[start-str end-str] (str/split l #"-")]]
                 [(parse-long start-str) (parse-long end-str)])
        ids (for [l (str/split-lines ids-str)]
              (bigint l))]
    {:ranges ranges
     :ids ids}))

(defn is-fresh? [id ranges]
  (some (fn [[start end]]
          (<= start id end))
        ranges))

(println "Part 1:"
         (let [{:keys [ranges ids]} (process-input (slurp "5input"))]
           (count (keep #(is-fresh? % ranges) ids))))

(defn ranges-overlap [[x-start x-end] [y-start y-end]]
  (and (<= x-start y-end)
       (<= y-start x-end)))

(defn merge-ranges [[x-start x-end] [y-start y-end]]
  [(min x-start y-start)
   (max x-end y-end)])

(def test-ranges (sort (:ranges (process-input test-input))))

(defn remove-overlaps [ranges]
  (if (< (count ranges) 2)
    ranges
    (let [r1 (first ranges), r2 (second ranges)]
      (if (ranges-overlap r1 r2)
        (remove-overlaps (cons (merge-ranges r1 r2)
                               (drop 2 ranges)))
        (cons r1 (remove-overlaps (rest ranges)))))))

(println "Part 2:"
         (->> (remove-overlaps (sort (:ranges (process-input (slurp "5input")))))
              (map (fn [[start end]]
                     (+ 1 (- end start))))
              (reduce +)))
