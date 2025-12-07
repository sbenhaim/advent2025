(ns twentyfive.four
  (:require [clojure.string :as str]))

(def input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")


(defn parse-matrix
  [input]
  (->> input
       str/split-lines
       (mapv #(mapv str %))))


(def roll? #{"@"})



(defn surrounds
  [matrix [row col]]
  (for [y (range (dec row) (+ row 2))
        x (range (dec col) (+ col 2))
        :when (not (and (= col x) (= row y)))]
    (get-in matrix [y x])))


(defn accessible?
  [matrix pos]
  (let [x (get-in matrix pos)
        ss (surrounds matrix pos)]
    (and (roll? x) (< (count (filter roll? ss)) 4))))


(comment (accessible? matrix [0 2]))

;; Part 1
(->>
 (let [matrix (parse-matrix (slurp "data/four.txt"))]
   (for [row (-> matrix count range)
         col (-> matrix first count range)]
     (accessible? matrix [row col])))
 (filter true?)
 count)


;; Part 2

(loop [matrix (parse-matrix (slurp "data/four.txt")) removed 0]
  (let [access
        (for [row (-> matrix count range)
              col (-> matrix first count range)]
          [[row col] (accessible? matrix [row col])])
        target (->> access
                    (drop-while (fn [[_ able]] (not able)))
                    ffirst)]
    (if-not target
      removed
      (recur (assoc-in matrix target "x") (inc removed)))))
