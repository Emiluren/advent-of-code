(def input (clojure.string/split-lines (slurp "input")))

;; Part 1
(def counts (map #(vals (frequencies %)) input))

(defn count-n [n coll]
  (count (filter #(some #{n} %) coll)))

(* (count-n 2 counts) (count-n 3 counts))

;; Part 2
(defn find-same [strings]
  (loop [x (first strings), xs (rest strings), found #{}]
    (cond
      (found x) x
      (not (seq xs)) nil
      :else (recur
             (first xs)
             (rest xs)
             (conj found x)))))

(defn string-skip [string i]
  (apply str (concat (take i string)
                     (drop (inc i) string))))

(some find-same
      (map (fn [i] (map #(string-skip % i) input))
           (iterate inc 0)))
