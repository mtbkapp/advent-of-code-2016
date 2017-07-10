(ns advent-of-code-2016.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(defn up
  [k]
  (when-not (#{1 2 3} k)
    (- k 3)))

(deftest test-up
  (is (nil? (up 1)))
  (is (nil? (up 2)))
  (is (nil? (up 3)))
  (is (= 1 (up 4)))
  (is (= 6 (up 9))))


(defn left
  [k]
  (when-not (#{1 4 7} k)
    (dec k)))

(deftest test-left
  (is (nil? (left 1)))
  (is (nil? (left 4)))
  (is (nil? (left 7)))
  (is (= 1 (left 2)))
  (is (= 7 (left 8))))


(defn right
  [k]
  (when-not (#{3 6 9} k)
    (inc k)))

(deftest test-right
  (is (nil? (right 3)))
  (is (nil? (right 6)))
  (is (nil? (right 9)))
  (is (= 2 (right 1)))
  (is (= 8 (right 7))))


(defn down
  [k]
  (when-not (#{7 8 9} k)
    (+ k 3)))

(deftest test-down
  (is (nil? (down 7)))
  (is (nil? (down 8)))
  (is (nil? (down 9)))
  (is (= 9 (down 6)))
  (is (= 4 (down 1))))

(def move-map
  {\L left
   \R right
   \U up
   \D down})

(defn move
  [k dir]
  (or ((move-map dir) k) k))

(deftest test-move
  (is (= 4 (move 5 \L)))
  (is (= 2 (move 5 \U)))
  (is (= 8 (move 5 \D)))
  (is (= 6 (move 5 \R)))
  (is (= 1 (move 1 \L)))
  (is (= 1 (move 1 \U)))
  (is (= 9 (move 9 \R)))
  (is (= 9 (move 9 \D))))

(defn solve-part-1
  [input]
  (loop [k 5 code [] [line & lines] input]
    (if line
      (let [nk (reduce move k line)]
        (recur nk (conj code nk) lines))
      code)))

(def test-input
  [[\U \L \L]
   [\R \R \D \D \D]
   [\L \U \R \D \L]
   [\U \U \U \U \D]])

(deftest test-solve-part-1
  (is (= [1 9 8 5] (solve-part-1 test-input))))

(defn read-part-1-input
  []
  (->> (slurp (io/resource "day2_part1.txt"))
       (string/split-lines)
       (map seq)))


