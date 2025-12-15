(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn process-input [input-str]
  (for [l (str/split-lines input-str)]
    (->> (str/split l #",")
         (mapv parse-long))))

(defn all-edges [vertices]
  (->> (for [v1 vertices
             v2 vertices
             :when (not (= v1 v2))]
         #{v1 v2})
       set
       (mapv vec)))

(defn square [x]
  (* x x))

(defn dist2-between [[[x1 y1 z1] [x2 y2 z2]]]
  (+ (square (- x1 x2))
     (square (- y1 y2))
     (square (- z1 z2))))

(defn init-disj-set [vertices]
  (->> (for [v vertices]
         {v v})
       (reduce merge)))

(defn find-root [disj-set v]
  (let [parent (disj-set v)]
    (if (= parent v)
      v
      (find-root disj-set parent))))

(defn union-disj-sets [disj-set e1 e2]
  (let [root1 (find-root disj-set e1)
        root2 (find-root disj-set e2)]
    (assoc disj-set root1 root2)))

(defn kruskal [vertices edges]
  (reduce (fn [disj-set [v1 v2]]
            (let [root1 (find-root disj-set v1)
                  root2 (find-root disj-set v2)]
              (if (= root1 root2)
                disj-set
                (union-disj-sets disj-set root1 root2))))
          (init-disj-set vertices)
          edges))

(def vertices (process-input (slurp "8input")))

(def disj-set (let [edges (sort-by dist2-between (all-edges vertices))]
                (kruskal vertices (take 1000 edges))))

(def all-sets
  (->> (for [v vertices]
         {(find-root disj-set v) #{v}})
       (reduce #(merge-with set/union %1 %2))))

(println "Part 1:"
         (reduce * (take 3 (reverse (sort (map count (vals all-sets)))))))
