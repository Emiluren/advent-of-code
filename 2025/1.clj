(require '[clojure.string :as str])

(def *input-lines* (str/split-lines (slurp "/home/emil/projekt/advent-of-code/2025/1input")))

(defn decode-ins [s]
  (let [num (parse-long (subs s 1))]
    (if (= \L (first s))
      (- num)
      num)))

(def *decoded-input* (mapv decode-ins *input-lines*))

(defn turn [pos x]
  (mod (+ pos x) 100))

(def *all-states*
  (reductions turn 50 *decoded-input*))

(println "Part 1:" (get (frequencies *all-states*) 0))

(defn sign [x]
  (if (>= 0 x)
    1
    -1))

;; Attempted rewrite doesn't give correct answer
#_(defn passes-of-0 [start n]
  (let [steps (repeat (abs n) (sign n))
        states (reductions turn start steps)]
    (get (frequencies states) 0 0)))

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

(def test-input [-68 -30 48 -5 60 -55 -1 -99 14 -82])

(println "Part 2:"
         (reduce + (map passes-of-0 *all-states* *decoded-input*)))
