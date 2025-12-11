(ns twentyfive.seven
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")


(defn parse
  [i]
  (let [[start & rst] (str/split-lines i)
        beam-start (count (take-while #(not= % \S) start))
        splitters (map
                   #(into #{} (map-indexed (fn [i c] (when (= c \^) i)) %))
                   rst)]
    [beam-start splitters]))


(comment
  (parse input))


(defn split [splits]
  (into #{} (mapcat #(vector (dec %) (inc %)) splits)))


(comment
  ;; Part 1
  (let [[beam-start splitters] (parse input #_(slurp "data/seven.txt"))]
    (loop [beams #{beam-start} [cur & rst] splitters n-splits 0]
      (if (nil? cur) [beams n-splits]
          (let [splits (set/intersection beams cur)
                skips  (set/difference beams cur)]
            (recur (set/union skips (split splits))
                   rst
                   (+ n-splits (count splits))))))))


(defn qsplit [beams splits]
  (let [ps (-> beams keys set)
        spls (set/intersection ps splits)]
    (reduce (fn [beams spl]
              (let [paths (beams spl)]
                (-> beams
                    (dissoc spl)
                    (update (dec spl) #(+ paths (or % 0)))
                    (update (inc spl) #(+ paths (or % 0))))))
            beams
            spls)))

(comment
  (qsplit {3 1 5 2 7 1} #{5 7}))

(comment
  (qsplit
   [5 7 3] #{5 7}))

(comment
   ;; Part 2
  (let [[beam-start splitters] (parse #_input (slurp "data/seven.txt"))]
    (loop [beams {beam-start 1} [cur & rst] splitters]
      (if (nil? cur) (println (reduce + (vals beams)))
          (recur (qsplit beams cur)
                 rst)))))
