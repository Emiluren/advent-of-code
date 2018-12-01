(ns day-9.core
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]))

(def input-data (slurp "input"))

(def garbage-parser
  (insta/parser (slurp "garbage-syntax")))

(defn filter-group-tree [& elements]
  (->> elements vec pop rest (take-nth 2) vec))

(def parsed-data
  (->> input-data
       garbage-parser
       (insta/transform
        {:random str
         :garbage str
         :group filter-group-tree})))

(defn calc-group-score [group base-score]
  (cond
    (string? group) 0
    (vector? group) (+ base-score
                       (reduce + (map #(calc-group-score % (inc base-score)) group)))))

(defn count-garbage [group]
  (cond
    (string? group) (+ (count (clojure.string/replace group #"!." ""))
                       -2)
    (vector? group) (reduce + (map count-garbage group))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



