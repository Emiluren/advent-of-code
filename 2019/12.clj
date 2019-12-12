(def input
  {:x [1 -4 -15 -17]
   :y [4 -1 -14 1]
   :z [4 19 12 10]})

(def test-input1
  {:x [-1 2 4 3]
   :y [0 -10 -8 5]
   :z [2 -7 8 -1]})

(def test-input2 [(v -8 -10 0) (v 5 5 10) (v 2 -7 3) (v 9 -8 -3)])

(defn initial-component-state [xs]
  {:pos xs :vel [0 0 0 0]})

(defn moons-from-positions [{:keys [x y z]}]
  {:x (initial-component-state x)
   :y (initial-component-state y)
   :z (initial-component-state z)})

(def start-state
  (moons-from-positions input))

(defn sign [x]
  (cond (> x 0) 1
        (< x 0) -1
        :else 0))

(defn apply-gravity-single [xs x]
  (reduce + (map #(sign (- % x)) xs)))

(defn apply-gravity-all [positions]
  (map (partial apply-gravity-single positions) positions))

(defn apply-velocity [positions velocities]
  (map + positions velocities))

(defn step-component [{:keys [pos vel]}]
  (let [acceleration (apply-gravity-all pos)
        new-vel (map + vel acceleration)]
    {:pos (apply-velocity pos new-vel)
     :vel new-vel}))

(defn step-state [{:keys [x y z]}]
  {:x (step-component x)
   :y (step-component y)
   :z (step-component z)})

(defn abs-sum [x y z]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn key-energy [key values]
  (apply map abs-sum (map key values)))

(defn calc-energy [{:keys [x y z]}]
  (reduce + (map * (key-energy :pos [x y z]) (key-energy :vel [x y z]))))

(defn solve-a []
  (calc-energy (nth (iterate step-state
                             start-state)
                    1000)))
