(ns vindinium.ai
  (:use [vindinium.pathfinder :only [breadth-first-search escape-enemy]]))

(defn ^:private compare-distance [enemy1 enemy2]
  (if (< (count enemy1) (count enemy2)) enemy1 enemy2))

(defn ^:private smaller-distance [a b]
  (cond (and a b) (compare-distance a b)
          a a
          b b
          :else nil))

(defn closest-enemy [closest]
  (let [enemy1-path (:enemy1 closest)
        enemy2-path (:enemy2 closest)
        enemy3-path (:enemy3 closest)
        enemy4-path (:enemy4 closest)]
    (reduce smaller-distance [enemy1-path enemy2-path enemy3-path enemy4-path])))

(defn ^:private enemy-2-sqr-away [enemy]
  (println "enemy" (count enemy) "squares away" enemy)
  (<= (count enemy) 2))

(defn go-towards-mine
  "Finds the nearest mine and goes towards it, unless HP is below 70 and a
  tavern is closer, in which case gets a drink first."
  [input]
  (let [search-result (breadth-first-search input :mine)
        life (:life (:hero input))
        enemy (closest-enemy search-result)]
    (if (and (> 70 life) (:tavern search-result))
      (first (:tavern search-result))
      (first (:mine search-result)))))

(defn go-towards-tavern
  "Goes towards the nearest tavern. If another hero is on a way, escapes the
  hero to avoid getting killed."
  [input]
  (let [search-result (breadth-first-search input :tavern)
        enemy (closest-enemy search-result)]
    (if (and enemy (enemy-2-sqr-away enemy))
      (escape-enemy input enemy)
      (first (:tavern search-result)))))

