(ns advent-of-code-2016.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))


(defn read-input
  []
  (->> (slurp (io/resource "day2_part1.txt"))
       (string/split-lines)
       (map seq)))

(defn coords->key
  [keypad [kx ky]]
  (get-in keypad [ky kx]))

(defmacro defmove
  [move-name key-names next-key]
  `(defn ~move-name
     [keypad# ~key-names]
     (let [nk# ~next-key]
       (when (coords->key keypad# nk#)
         ~next-key))))

(defmove left
  [kx ky]
  [(dec kx) ky])

(defmove right
  [kx ky]
  [(inc kx) ky])

(defmove up
  [kx ky]
  [kx (dec ky)])

(defmove down
  [kx ky]
  [kx (inc ky)])

(def move-map
  {\U up 
   \L left
   \R right
   \D down})

(defn move
  [keypad k-coord dir]
  (or ((move-map dir) keypad k-coord) k-coord))

(defn solve*
  [keypad start input]
  (loop [k start code [] [line & lines] input]
    (if line
      (let [next-k (reduce (partial move keypad) k line)]
        (recur next-k (conj code next-k) lines))
      code)))

(defn solve
  [keypad start input]
  (map (partial coords->key keypad)
       (solve* keypad start input)))


;; Part 1

(def part1-keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(defn solve-part-1
  [input]
  (solve part1-keypad [1 1] input))

(def test-input
  [[\U \L \L]
   [\R \R \D \D \D]
   [\L \U \R \D \L]
   [\U \U \U \U \D]])

(deftest test-solve-part-1
  (is (= [1 9 8 5] (solve-part-1 test-input))))

#_(solve-part-1 (read-input))

;; Part 2

(def part2-keypad
  [[nil nil 1 nil nil]
   [nil  2  3  4  nil]
   [ 5   6  7  8   9]
   [nil \A \B \C  nil]
   [nil nil \D nil nil]])

(defn solve-part2
  [input]
  (solve part2-keypad [0 2] input))

(deftest test-solve-part2
  (is (= [5 \D \B 3] (solve-part2 test-input))))

#_(solve-part2 (read-input))

