(require '[clojure.string :as str])

(defn parse-input [input-str]
  (for [l (str/split-lines input-str)]
    (mapv parse-long (str/split l #","))))

(defn all-squares [coords]
  (loop [coords coords
         squares []]
    (if (not (seq (rest coords)))
      squares
      (recur (rest coords)
             (apply conj squares
                    (for [c2 (rest coords)]
                      [(first coords) c2]))))))

(defn square-area [[[x1 y1] [x2 y2]]]
  (* (abs (+ 1 (- x1 x2)))
     (abs (+ 1 (- y1 y2)))))

(println "Part 1:"
         (->> (parse-input (slurp "9input"))
              all-squares
              (map square-area)
              (reduce max)))
