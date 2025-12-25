(ns twentyfive.eight
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(def input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")


(defn parse
  [i]
  (->> i
       str/split-lines
       (map #(str/split % #","))
       (map #(map parse-long %))
       ))




(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2)
                (Math/pow (- z2 z1) 2))))

(comment
  (set/subset? #{1 2 3} #{1 2}))


(defn sort-first
  [c]
  (sort-by first c))


(defn shortest
  [nodes]
  (->>
   (for [n1    nodes
         n2    nodes
         :when (not= n1 n2)]
     [(distance n1 n2) #{n1 n2}])
   distinct
   sort-first))


(comment
  (take 40 (shortest (parse input))))


(defn wire
  [wires circuits]
  (let [existing-a (get circuits (first wires) #{})
        existing-b (get circuits (second wires) #{})
        existing (set/union existing-a existing-b)
        new (set/union existing wires)]
    [(reduce
      (fn [m k] (assoc m k new))
      circuits
      new)
     (count new)]))


(comment
  (wire #{3 2} {0 #{0 1} 1 #{0 1}}))


(comment
  ;; Part 1
  (->>
   (loop [pairs (-> #_input (slurp "data/eight.txt") parse shortest) circuits {} iters 1000]
     (if (zero? iters)
       circuits
       (let [[[_ wires] & rst] pairs] 
         (recur rst (wire wires circuits) (dec iters)))))
   vals
   distinct
   (map count)
   sort
   reverse
   (take 3)
   (reduce *)))


;; Part 2
(defn wire2
  [wires circuits]
  (let [existing-a (get circuits (first wires) #{})
        existing-b (get circuits (second wires) #{})
        existing (set/union existing-a existing-b)
        new (set/union existing wires)]
    [(reduce
      (fn [m k] (assoc m k new))
      circuits
      new)
     (count new)]))


(let [input (slurp "data/eight.txt")
      nodes (parse input)
      n-nodes (count nodes)]
  (loop [pairs (shortest nodes) circuits {}]
    (let [[[_ wires] & rst]           pairs
          [nxt-circuits circuit-size] (wire2 wires circuits)] 
      (if (= n-nodes circuit-size)
        wires
        (recur rst nxt-circuits)))))
