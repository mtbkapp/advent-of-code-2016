(ns advent-of-code-2016.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))


(defn read-input
  []
  (->> (io/resource "day3_part1.txt")
       (slurp)
       (string/split-lines)
       (map (comp #(map (fn [x] (Integer/parseInt x)) %)
                  rest
                  (partial re-find #"^\s*(\d+)\s+(\d+)\s+(\d+)\s*$")))))

(defn triangle?
  [sides]
  (let [[x y z] (sort sides)]
    (> (+ x y) z)))

(deftest test-triangle?
  (is (triangle? [3 4 5]))
  (is (triangle? [3 5 4]))
  (is (triangle? [4 3 5]))
  (is (triangle? [4 5 3]))
  (is (triangle? [5 3 4]))
  (is (triangle? [5 4 3]))
  (is (not (triangle? [1 2 3])))
  (is (not (triangle? [1 3 2])))
  (is (not (triangle? [2 1 3])))
  (is (not (triangle? [2 3 1])))
  (is (not (triangle? [3 1 2])))
  (is (not (triangle? [3 2 1]))))

(defn count-triangles
  [input]
  (count (filter triangle? input)))

#_(count-triangles (read-input))


(def vertical-triangles
  [[101 301 501]
   [102 302 502]
   [103 303 503]
   [201 401 601]
   [202 402 602]
   [203 403 603]])

(defn group-vertically
  [input]
  (->> (partition 3 input)
       (mapcat (fn [group]
                 [(map first group)
                  (map second group)
                  (map #(nth % 2) group)]))))

(deftest test-group-vertically
  (is (= #{[101 102 103] [301 302 303] [501 502 503]
           [201 202 203] [401 402 403] [601 602 603]}
         (set (group-vertically vertical-triangles)))))

(defn count-vertical-triangles
  [input]
  (count (filter triangle? (group-vertically input))))

#_(count-vertical-triangles (read-input))
