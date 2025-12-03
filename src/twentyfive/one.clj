(ns twentyfive.one
  (:require [clojure.string :as str]))

(def input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defn parse
  [i]
  (let [[dir & num] i
        n (parse-long (apply str num))]
    (if (= \L dir)
      (- n)
      n)))

(comment
  (parse (-> input str/split-lines first)))

(def dial 100)

(defn pos
  [start rot]
  (mod (+ start rot) dial))


(comment
  (pos 50 -68)
  (pos 50 (parse (-> input str/split-lines first))))


;; Part one
(comment
  (let [input input
        input (slurp "data/one.txt")
        rotations (map parse (str/split-lines input))
        positions (reductions (fn [p r] (pos p r)) 50 rotations)]
    (count (filter zero? positions))))

;; Part two

(defn cycs
  [start rot]
  (let [start (cond
                (zero? start) 0
                (neg? rot) (- dial start)
                :else start)]
    (quot (+ start (abs rot)) dial)))

(comment
  (cycs 5 -10)
  (cycs 5 -105)
  (cycs 0 100))

;; Part one
(comment
  (let [input input
        input (slurp "data/one.txt")
        rotations (map parse (str/split-lines input))
        positions (reduce (fn [[p cnt] r] [(pos p r) (+ cnt (cycs p r))]) [50 0]  rotations)]
    (second positions)))
