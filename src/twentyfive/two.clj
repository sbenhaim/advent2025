(ns twentyfive.two
  (:require [clojure.string :as str]))

(def input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,
38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")


(defn parse-range
  [i]
  (let [[low high] (str/split i #"-")
        [low high] (map parse-long [low high])]
    (range low (inc high))))


(comment
  (parse-range "11-22"))


(defn invalid?
  [n]
  (let [nstr (str n)
        len (count nstr)
        half (/ len 2)]
    (if (odd? len)
      false
      (= (take half nstr) (drop half nstr)))))

(comment
  (invalid? 123123))


(comment
  (let [input (slurp "data/two.txt")]
    (-> input
        (str/split #",")
        (->>
         (transduce (comp
                     (map str/trim)
                     (mapcat parse-range)
                     (filter invalid?))
                    +)))))



(defn invalid2?
  [n]
  (boolean
   (re-find #"^(.*)(\1)+$" (str n))))


(comment
  (invalid2? 12)
  (invalid2? 1212)
  (invalid2? 121112)
  (invalid2? 121212)
  (invalid2? 12312312)
  (invalid2? 123123123123))


(comment
  (let [input (slurp "data/two.txt")]
    (-> input
        (str/split #",")
        (->>
         (transduce (comp
                     (map str/trim)
                     (mapcat parse-range)
                     (filter invalid2?))
                    +))
        println)))
