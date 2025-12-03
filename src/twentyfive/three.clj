(ns twentyfive.three
  (:require [clojure.string :as str]))

(def input "987654321111111
811111111111119
234234234234278
818181911112111")


(defn parse
  [i]
  (transduce (comp (map str) (map parse-long)) conj i))


(comment
  (parse (first (str/split-lines input))))

(defn max-joltage
  [ns]
  (let [tens (apply max (butlast ns))
        [tens & remaining] (drop-while (complement #{tens}) ns)
        ones (apply max remaining)]
    (parse-long (str tens ones))))





(comment
  (->> (slurp "data/three.txt")
       str/split-lines
       (transduce (comp (map parse) (map max-joltage)) +)))

(max-joltage [1 8 9 7 2])

;; Part 2

(defn max-joltage-part-2
  [nums n-digits]
  (loop [remaining nums found []]
    (if (= n-digits (count found))
      (parse-long (apply str found))
      (let [n-rem      (count remaining)
            n-found    (count found)
            to-find    (- n-digits n-found)
            candidates (take (- n-rem (dec to-find)) remaining)
            next-found (apply max candidates)
            remaining  (drop-while (complement #{next-found}) remaining)]
        (if (= (count remaining) to-find)
          (recur [] (concat found remaining))
          (recur (rest remaining) (conj found next-found)))))))

(comment
  (-> "811111111111119" parse (max-joltage-part-2 12)))


(comment
  (->> (slurp "data/three.txt")
       str/split-lines
       (transduce (comp (map parse) (map #(max-joltage-part-2 % 12))) +)
       println))
