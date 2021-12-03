(ns core
  "
  Advent of Code
  https://adventofcode.com/

  (def y 2021)
  "
  (:require [clojure.string :as str]))


(defn get-data [parse-fn file]
  (with-open [rdr (clojure.java.io/reader (str "data/2021/" file))]
    (mapv parse-fn (line-seq rdr))))

(defn pp [day r1 r2]
  (println (str "Day " day))
  (println "1: -> " r1)
  (println "2: -> " r2))

(defn day1 []
  (let [data (get-data #(Long/parseLong %) "1.txt")
        process-fn #(->> (map - % (next %))
                         (filter neg?)
                         (count))]
    (pp 1
      (process-fn data)
      (process-fn
        (map + data (next data) (nnext data))))))

(defn day2 []
  (let [xs (get-data
             #(let [[c v] (str/split % #" ")]
                [c (Long/parseLong v)]) "2.txt")
        [a b] (reduce
                (fn [[forward depth] [c v]]
                  (case c
                    "forward" [(+ forward v) depth]
                    "up" [forward (- depth v)]
                    "down" [forward (+ depth v)]))
                [0 0] xs)
        [c d _] (reduce
                  (fn [[depth horizontal aim] [c v]]
                    (case c
                      "forward" [(+ depth (* aim v)) (+ horizontal v) aim]
                      "up" [depth horizontal (- aim v)]
                      "down" [depth horizontal (+ aim v)]))
                  [0 0 0] xs)]
    (pp 2 (* a b) (* c d))))

(defn day-3 [])