(ns vindinium.pathfinder
  (:require [clojure.math.numeric-tower :as math]))

(defn- ^{:testable true} target-aqquired? [found target]
  (boolean (get found target)))

(defn- ^{:testable true} closest-mine-located? [found tiles i id]
  (and (not (:mine found))
       (= :mine (:tile (get tiles i)))
       (not= id (:of (get tiles i)))))

(defn- ^{:testable true} closest-tavern-located? [found tiles i]
  (and (not (:tavern found))
       (= :tavern (:tile (get tiles i)))))

(defn- enemy [tiles i]
  (keyword (str "enemy" (:id (get tiles i)))))

(defn- ^{:testable true} enemy-located? [found tiles i id]
  (and (= :hero (:tile (get tiles i)))
       (not= id (:id (get tiles i)))
       (not (contains? found (enemy tiles i)))))

(defn- ^{:testable true} tavern-or-mine? [tiles i]
  (let [tile-type (:tile (get tiles i))]
    (or (= :mine tile-type)
        (= :tavern tile-type))))

(defn- ^{:testable true} label [direction]
  (let [labels ["north" "west" "south" "east"]]
    (get labels direction)))

(defn- ^{:testable true} north [i board-size]
  (- i board-size))

(defn- ^{:testable true} west [i]
  (dec i))

(defn- ^{:testable true} south [i board-size]
  (+ i board-size))

(defn- ^{:testable true} east [i]
  (inc i))

(defn- ^{:testable true} shift-i-to-direction [i size direction]
  (get [(north i size) (west i) (south i size) (east i)] direction))

(defn- ^{:testable true} all-directions-explored? [direction]
  (or (> direction 3) (< direction 0)))

(defn- ^{:testable true} tile-inside-borders? [tiles i start]
  (let [tile-count (count tiles)
        size (math/sqrt tile-count)]
    (and (>= i 0)
         (< i tile-count)
         (if (zero? (mod start size)) (not= i (dec start)) true)
         (if (= (dec size) (mod start size)) (not= i (inc start)) true))))

(defn- ^{:testable true} tile-not-previously-visited? [tiles i]
  (not (:visited (get tiles i))))

(defn- ^{:testable true} tile-not-a-wall? [tiles i]
  (not= :wall (:tile (get tiles i))))

(defn- ^{:testable true} tile-not-a-mine? [tiles i]
  (not= :mine (:tile (get tiles i))))

(defn- ^{:testable true} tile-not-a-hero? [tiles i]
  (not= :hero (:tile (get tiles i))))

(defn- ^{:testable true} direction-ok-for-exploring? [tiles i start]
  (and (tile-inside-borders? tiles i start)
       (tile-not-previously-visited? tiles i)
       (tile-not-a-wall? tiles i)))

(defn- ^{:testable true} direction-ok-for-escaping? [tiles i start]
  (and (tile-inside-borders? tiles i start)
       (tile-not-a-wall? tiles i)
       (tile-not-a-mine? tiles i)
       (tile-not-a-hero? tiles i)))

(defn- ^{:testable true} visited [tiles index]
  (assoc-in tiles [index :visited] true))

(defn- ^{:testable true} queue-with-new-node [nodes path direction i]
  (conj nodes {:path (conj path (label direction))
               :i i}))

(defn- ^{:testable true} i-from [coord size]
  (+ (* (second coord) size) (first coord)))

(defn- explore-neighbouring-nodes [tiles size nodes i path]
  (let [shift (partial shift-i-to-direction i size)]
    (loop [direction 0
           acc-tiles tiles
           acc-nodes (vec (rest nodes))]
      (cond (all-directions-explored? direction)
              {:tiles acc-tiles :nodes acc-nodes}
            (direction-ok-for-exploring? acc-tiles (shift direction) i)
              (recur (inc direction)
                     (visited acc-tiles (shift direction))
                     (queue-with-new-node acc-nodes path direction (shift direction)))
            :else (recur (inc direction) acc-tiles acc-nodes)))))

(defn- lookup-closest [found tiles size nodes id target]
  (let [node (first nodes)
        path (:path node)
        i (:i node)]
    (cond (target-aqquired? found target)
            found
          (closest-mine-located? found tiles i id)
            (recur (assoc found :mine path) tiles size (vec (rest nodes)) id target)
          (closest-tavern-located? found tiles i)
            (recur (assoc found :tavern path) tiles size (vec (rest nodes)) id target)
          (enemy-located? found tiles i id)
            (recur (assoc found (enemy tiles i) path) tiles size nodes id target)
          (tavern-or-mine? tiles i)
            (recur found tiles size (vec (rest nodes)) id target)
          :else
          (let [visited (explore-neighbouring-nodes tiles size nodes i path)]
            (recur found (:tiles visited) size (:nodes visited) id target)))))

(defn- back-away [tiles size enemy i]
  (let [shift (partial shift-i-to-direction i size)
        direction (atom (first enemy))]
    (dotimes [n 4]
      (if (and (direction-ok-for-escaping? tiles (shift n) i)
               (not= (first enemy) (label n)))
        (swap! direction (fn [x] (label n)))))
    (println "Escaping to", @direction)
    @direction))

(defn- take-action [action input target]
  (let [board (:board (:game input))
        size (:size board)
        tiles (:tiles board)
        i (i-from (:pos (:hero input)) size)
        id (:id (:hero input))]
    (if (= :search action)
      (lookup-closest {} tiles size [{:path [] :i i}] id target)
      (back-away tiles size target i))))

(defn breadth-first-search
  "Takes a target to go towars (:tavern for example) and finds the nearest one
  using breadth first search. Observes and records the locations of other points
  of interests on the way."
  [input target]
  (take-action :search input target))

(defn escape-enemy
  "Backs away from the nearest enemy to avoid getting hit with low health."
  [input enemy]
  (take-action :escape input enemy))
