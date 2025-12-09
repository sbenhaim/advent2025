(ns twentyfive.six
  (:require [clojure.string :as str]))

(def input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
")


(defn parse
  [i]
  (let [rows (str/split-lines i)
        rows (for [r rows] (str/split (str/trim r) #" +"))
        nums (map #(map parse-long %) (butlast rows))
        transposed (apply mapv vector nums)
        ops (map (comp eval symbol) (last rows))]
    (map (fn [op nums] (apply op nums))
         ops transposed)))


;; Part one
(comment
  (->>
   (parse (slurp "data/six.txt"))
   (reduce +)
   println))

(defn col->num
  [c]
  (parse-long
   (apply str c)))


;; Part two
(comment
  (let [lines (str/split-lines (slurp "data/six.txt"))
        cols  (for [i (-> lines first count range)]
                (map #(nth % i) (butlast lines)))
        ops   (map (comp eval symbol) (str/split (last lines) #" +" ))

        cols   (for [c cols]
                 (map (fn [c] (if (= \space nil) 0 (-> c str parse-long))) c))
        groups (->> (partition-by #(every? nil? %) cols)
                    (remove #(every? nil? (first %))))
        nums   (for [g groups]
                 (map col->num g))
        
        addends (map (fn [op nums] (apply op nums))
                     ops nums)]
    (println
     (reduce + addends))))
