(def *input-lines* (str/split-lines (slurp "1input")))

(defn decode-ins [s]
  (let [num (Integer/parseInt (subs s 1))]
    (if (= \L (first s))
      (- num)
      num)))

(def *decoded-input* (mapv decode-ins *input-lines*))

(def *all-states*
  (reductions #(mod (+ %1 %2) 100) 50 *decoded-input*))

(println "Part 1:" (get (frequencies *all-states*) 0))
