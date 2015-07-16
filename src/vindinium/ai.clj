(ns vindinium.ai
  (:use [vindinium.pathfinder :only [breadth-first-search]]))

(defn ^:private compare-distance [enemy1 enemy2]
  (if (< (count (get enemy1 1)) (count (get enemy2 1))) enemy1 enemy2))

(defn ^:private smaller-distance [a b]
  (cond (and a b) (compare-distance a b)
          a a
          b b
          :else nil))

(defn closest-enemy [closest]
  (let [enemy1 (:enemy1 closest)
        enemy2 (:enemy2 closest)
        enemy3 (:enemy3 closest)
        enemy4 (:enemy4 closest)
        info1 (if enemy1 [1 enemy1] nil)
        info2 (if enemy2 [2 enemy2] nil)
        info3 (if enemy3 [3 enemy3] nil)
        info4 (if enemy4 [4 enemy4] nil)]
    (if (or info1 info2 info3 info4) (println info1 info2 info3 info4))
    (reduce smaller-distance [info1 info2 info3 info4])))

(defn go-towards-mine [input]
  (let [search (breadth-first-search input :mine)
        life (:life (:hero input))
        enemy (closest-enemy search)]
    (if (and (>= 70 life) (:tavern search))
      (first (:tavern search))
      (first (:mine search)))))

(defn go-towards-tavern [input]
  (let [search (breadth-first-search input :tavern)
        enemy (closest-enemy search)]
    (first (:tavern search))))

