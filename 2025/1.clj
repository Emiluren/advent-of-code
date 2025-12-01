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

;; Could probably be massively simplified using modular arithmetic
(defn passes-of-0 [start n]
  (loop [pos start
         count-0 0
         i 0]
    (if (= (abs n) i)
      count-0
      (let [new-pos (mod (if (< n 0)
                           (- pos 1)
                           (+ pos 1))
                         100)]
        (recur new-pos
               (if (= 0 new-pos)
                 (+ count-0 1)
                 count-0)
               (+ i 1))))))

(println "Part 2:"
         (reduce + (map passes-of-0 *all-states* *decoded-input*)))
