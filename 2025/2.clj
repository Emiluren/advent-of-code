(defn parse-input [input]
  (->> (str/split (str/trim input) #",")
       (mapv #(mapv bigint (str/split % #"-")))))

(def *input-ranges*
  (parse-input (slurp "2input")))

(defn is-repeat? [full-s sub-s]
  (let [len-factor (quot (count full-s) (count sub-s))
        repeated-sub (apply str (repeat len-factor sub-s))]
    (when (= full-s repeated-sub)
      sub-s)))

(defn is-invalid? [s]
  (let [substrings (map #(subs s 0 %) (range 1 (+ 1 (quot (count s) 2))))]
    (some #(is-repeat? s %) substrings)))

(defn is-invalid-twice? [s]
  (let [half-len (quot (count s) 2)]
    (and (even? (count s))
         (= (subs s 0 half-len)
            (subs s half-len)))))

(defn invalids [predicate ranges]
  (for [[start end] ranges
        x (range start (+ 1 end))
        :when (predicate (str x))]
    x))

(def *test-input* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

#_(invalids is-invalid-twice? (parse-input *test-input*))

(println "Part 1"
         (reduce + (invalids is-invalid-twice? *input-ranges*)))

(println "Part 2"
         (reduce + (invalids is-invalid? *input-ranges*)))
