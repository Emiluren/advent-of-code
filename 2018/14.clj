(def start-state
  {:board [3 7]
   :i1 0
   :i2 1})

(defn conj-new-scores [{:keys [board i1 i2]}]
  (let [score-sum (+ (board i1) (board i2))]
    (if (> score-sum 9)
      (conj board (quot score-sum 10) (mod score-sum 10))
      (conj board score-sum))))

(defn run-iteration [{:keys [board i1 i2], :as state}]
  (let [new-board (conj-new-scores state)]
    (assoc state
           :board new-board
           :i1 (mod (+ i1 1 (board i1)) (count new-board))
           :i2 (mod (+ i2 1 (board i2)) (count new-board)))))

(defn run-until-length [len]
  (loop [state start-state]
    (if (>= (count (:board state)) len)
      state
      (recur (run-iteration state)))))

(def input 990941)

(defn part-1 []
  (take 10 (drop input (:board (run-until-length (+ input 10))))))

(def input-sequence [9 9 0 9 4 1])

(defn index-of-seq [s]
  (loop [state start-state
         index 0]
    (cond
      (< (count (:board state)) (+ index 10))
      (recur (run-iteration state) index)

      (= (subvec (:board state) index (+ index (count s))) s)
      index

      :else (recur state (inc index)))))

(defn part-2 []
  (index-of-seq input-sequence))
