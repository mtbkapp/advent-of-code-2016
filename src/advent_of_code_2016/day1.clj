(ns advent-of-code-2016.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(defn parse-input
  [input]
  (map (juxt first (comp #(Integer/parseInt %)
                         #(string/replace (apply str %) #"\s*" "")
                         rest)) 
       (string/split input #",\s*")))

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
   :route [[0 0]]})

(defn heading
  [{[direction] :compass :as state}]
  direction)

(defn move
  [{:keys [route] :as state} units]
  (let [[x y] (last route)]
    (update state :route
            conj
            (case (heading state) 
              :N [x (+ y units)]
              :E [(+ x units) y]
              :S [x (- y units)]
              :W [(- x units) y]))))

(defn sim-moves
  [moves]
  (reduce (fn [state [direction units]]
            state
            (-> ((case direction \R turn-right \L turn-left) state)
                (move units)))
          initial-state
          moves))

(defn blocks
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn last-position
  [state]
  (->> state :route last))

(defn solve-part-1
  ([] (solve-part-1 (parse-input (slurp (io/resource "day1_input.txt")))))
  ([moves]
   (->> (sim-moves moves)
        :route
        last
        blocks)))

#_(solve-part-1)

(deftest test-left-loop
  (is (zero? (solve-part-1 [[\L 10] [\L 10] [\L 10] [\L 10]]))))

(deftest test-right-loop
  (is (zero? (solve-part-1 [[\R 10] [\R 10] [\R 10] [\R 10]]))))

(deftest test-up-zig-zag
  (is (= 20 (solve-part-1 [[\R 10] [\L 10] [\L 10] [\R 10]]))))

(deftest test-left-zig-zag
  (is (= 50 (solve-part-1 [[\L 10] [\L 10] [\R 10] [\R 10] [\L 10] [\L 10] [\R 10]]))))

(defn find-first-reoccuring-position
  [{route :route :as state}]
  (loop [[r & rs] route seen-positions #{}]
    (when r
      (if (contains? seen-positions r)
        r
        (recur rs (conj seen-positions r))))))

(deftest test-find-first-reoccuring-position
  (is (= [2 0] (find-first-reoccuring-position
                 (sim-moves [[\R 2] [\L 5] [\L 5] [\L 5] [\L 5] [\R 2] [\R 2] [\R 2]])))))

(defn solve-part-2
  []
  (blocks (find-first-reoccuring-position
            (sim-moves (parse-input (slurp (io/resource "day1_input.txt")))))))

#_(solve-part-2)

