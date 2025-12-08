(ns twentyfive.five
  (:require [clojure.string :as str]))

(def input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")


(defn parse-input
  [input]
  (let [[fresh-txt ids-txt] (str/split input #"\n\n")
        fresh-ranges (re-seq #"(\d+)-(\d+)" fresh-txt)
        fresh-ranges (map (fn [[_ & ns]] (mapv parse-long ns)) fresh-ranges)
        ids (->> ids-txt str/split-lines (map str/trim) (map parse-long))]
    [fresh-ranges ids]))


;; Part 1
(let [[fresh-ranges ids] (parse-input #_input (slurp "data/five.txt"))
      fresh-checks (map (fn [[l h]] (fn [v] (<= l v h))) fresh-ranges)
      check (apply some-fn fresh-checks)]
  (count
   (filter check ids)))

;; Part 2

(defn check-range [[l h] v]
  (<= l v h))

(defn overlap? [r1 r2]
  (or (check-range r2 (first r1))
      (check-range r2 (second r1))
      (check-range r1 (first r2))
      (check-range r1 (second r2))))

(comment (overlap? [3 4] [1 5]))

(defn simplify-ranges?
  [r1 r2]
  (when (overlap? r1 r2)
    (let [[l1 h1] r1
          [l2 h2] r2]
      [(min l1 l2) (max h1 h2)])))


(defn add-range
  [rs r]
  (loop [[r2 & r2s] rs]
    (if (nil? r2) (conj rs r)
        (if-let [r3 (simplify-ranges? r r2)]
          (-> rs
              (disj r2)
              (conj r3))
          (recur r2s)))))


(defn simplify-ranges
  [rs]
  (reduce (fn [rs r] (add-range rs r)) #{} (sort rs)))


(let [[rs _] (parse-input #_input (slurp "data/five.txt"))
      srs (simplify-ranges rs)]
  (->> srs
       (map (fn [[l h]] (inc (- h l))))
       (reduce +)
       (println)))


