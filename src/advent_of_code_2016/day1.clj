(ns advent-of-code-2016.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(defn parse-input
  [input]
  (map (juxt first (comp #(Integer/parseInt (apply str %))
                         rest)) 
       (string/split input #",\s*")))

(parse-input (slurp (io/resource "day1_input.txt")))


(defn turn-right
  [{:keys [compass] :as state}]
  (assoc state :compass 
         (concat (rest compass) [(first compass)])))

(defn turn-left
  [{:keys [compass] :as state}]
  (assoc state :compass 
         (cons (last compass) (butlast compass))))

(def initial-state 
  {:compass [:N :E :S :W]
   :position [0 0]})

(defn heading
  [{[direction] :compass :as state}]
  direction)

(defn move
  [{[x y] :position :as state} units]
  (assoc state :position
         (case (heading state) 
           :N [x (+ y units)]
           :E [(+ x units) y]
           :S [x (- y units)]
           :W [(- x units) y])))

(defn sim-moves
  [moves]
  (reduce (fn [state [direction units]]
            state
            (-> ((case direction \R turn-right \L turn-left) state)
                (move units)))
          initial-state
          moves))

(defn blocks
  [state]
  (->> state :position (apply +)))

(defn solve
  []
  (blocks (sim-moves (parse-input (slurp (io/resource "day1_input.txt"))))))

(deftest test-left-loop
  (is (zero? (blocks (sim-moves [[\L 10] [\L 10] [\L 10] [\L 10]])))))

(deftest test-right-loop
  (is (zero? (blocks (sim-moves [[\R 10] [\R 10] [\R 10] [\R 10]])))))

(deftest test-up-zig-zag
  (is (= 20 (blocks (sim-moves [[\R 10] [\L 10] [\L 10] [\R 10]])))))

(deftest test-left-zig-zag
  (is (= 50 (blocks (sim-moves [[\L 10] [\L 10] [\R 10] [\R 10] [\L 10] [\L 10] [\R 10]])))))


#_(solve)
