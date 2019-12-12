(defn v [x y z]
  {:x x, :y y, :z z})

(def input [{:x 1, :y 4, :z 4}
            {:x -4, :y -1, :z 19}
            {:x -15, :y -14, :z 12}
            {:x -17, :y 1, :z 10}])

(def test-input1 [{:x -1 :y 0 :z 2}
                  {:x 2 :y -10 :z -7}
                  {:x 4 :y -8 :z 8}
                  {:x 3 :y 5 :z -1}])

(def test-input2 [(v -8 -10 0) (v 5 5 10) (v 2 -7 3) (v 9 -8 -3)])

(defn moon-from-pos [p]
  {:pos p
   :vel {:x 0, :y 0, :z 0}})

(def start-state (mapv moon-from-pos input))

(defn sign [x]
  (cond (> x 0) 1
        (< x 0) -1
        :else 0))

(defn apply-gravity-axis [m1 m2 axis]
  (update-in m1
             [:vel axis]
             +
             (sign (- (get-in m2 [:pos axis])
                      (get-in m1 [:pos axis])))))

(defn apply-gravity-all [moons]
  (map (fn [m1]
         (reduce (fn [m1 m2]
                   (-> m1
                       (apply-gravity-axis m2 :x)
                       (apply-gravity-axis m2 :y)
                       (apply-gravity-axis m2 :z)))
                 m1
                 moons))
       moons))

(defn apply-velocity [moons]
  (map (fn [m]
         (-> m
             (update-in [:pos :x] + (get-in m [:vel :x]))
             (update-in [:pos :y] + (get-in m [:vel :y]))
             (update-in [:pos :z] + (get-in m [:vel :z]))))
       moons))

(defn sum-components [{:keys [x y z]}]
  (+ (Math/abs x)
     (Math/abs y)
     (Math/abs z)))

(defn moon-energy [{:keys [pos vel]}]
  (* (sum-components pos) (sum-components vel)))

(defn calc-energy [moons]
  (reduce + (map moon-energy moons)))

(defn solve-a []
  (calc-energy (nth (iterate (comp apply-velocity
                                   apply-gravity-all)
                             start-state)
                    1000)))
