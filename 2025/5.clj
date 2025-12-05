(require '[clojure.string :as str])

(def test-input "3-5
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
                 [(bigint start-str) (bigint end-str)])
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
