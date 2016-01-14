(ns vindinium.pathfinder)

(defn ^:private target-aqquired? [found target]
  (get found target))

(defn ^:private closest-mine-located? [found tiles i id]
  (and (not (:mine found))
       (= :mine (:tile (get tiles i)))
       (not= id (:of (get tiles i)))))

(defn ^:private closest-tavern-located? [found tiles i]
  (and (not (:tavern found))
       (= :tavern (:tile (get tiles i)))))

(defn ^:private enemy [tiles i]
  (keyword (str "enemy" (:id (get tiles i)))))

(defn ^:private enemy-located? [found tiles i id]
  (and (= :hero (:tile (get tiles i)))
       (not= id (:id (get tiles i)))
       (not (contains? found (enemy tiles i)))))

(defn ^:private tavern-or-mine? [tiles i]
  (let [tile-type (:tile (get tiles i))]
    (or (= :mine tile-type)
        (= :tavern tile-type))))

(defn ^:private label [direction]
  (let [labels ["north" "west" "south" "east"]]
    (get labels direction)))

(defn ^:private north [i board-size]
  (- i board-size))

(defn ^:private west [i]
  (dec i))

(defn ^:private south [i board-size]
  (+ i board-size))

(defn ^:private east [i]
  (inc i))

(defn ^:private shift-i-to-direction [i size direction]
  (get [(north i size) (west i) (south i size) (east i)] direction))

(defn ^:private all-directions-explored? [direction]
  (= direction 4))

(defn ^:private tile-inside-borders? [coord size direction]
  (let [x (first coord)
        y (second coord)]
    (get [(pos? y) (pos? x) (< y (dec size)) (< x (dec size))] direction)))

(defn ^:private shift-coordinates [coord direction]
  (let [x (first coord)
        y (second coord)]
    (get [[x (dec y)] [(dec x) y] [x (inc y)] [(inc x) y]] direction)))

(defn ^:private tile-not-previously-visited? [tiles i]
  (not (:visited (get tiles i))))

(defn ^:private tile-not-a-wall? [tiles i]
  (not= :wall (:tile (get tiles i))))

(defn ^:private tile-not-a-mine? [tiles i]
  (not= :mine (:tile (get tiles i))))

(defn ^:private tile-not-a-hero? [tiles i]
  (not= :hero (:tile (get tiles i))))

(defn ^:private direction-ok-for-exploring? [coord size shift tiles direction]
  (and (tile-inside-borders? coord size direction)
       (tile-not-previously-visited? tiles (shift direction))
       (tile-not-a-wall? tiles (shift direction))))

(defn ^:private direction-ok-for-escaping? [coord size shift tiles direction]
  (and (tile-inside-borders? coord size direction)
       (tile-not-a-wall? tiles (shift direction))
       (tile-not-a-mine? tiles (shift direction))
       (tile-not-a-hero? tiles (shift direction))))

(defn ^:private mark-tile-as-visited [tiles index]
  (assoc-in tiles [index :visited] true))

(defn ^:private add-node-to-queue [nodes direction coord n]
  (let [new-coords (partial shift-coordinates coord)]
    (conj nodes {:direction (conj direction (label n))
                 :coord (new-coords n)})))

(defn ^:private explore-neighbouring-nodes [tiles size nodes coord i dir]
  (let [shift (partial shift-i-to-direction i size)]
    (loop [n 0
           acc-tiles tiles
           acc-nodes (vec (rest nodes))]
      (cond (all-directions-explored? n)
              {:tiles acc-tiles :nodes acc-nodes}
            (direction-ok-for-exploring? coord size shift acc-tiles n)
              (recur (inc n)
                     (mark-tile-as-visited acc-tiles (shift n))
                     (add-node-to-queue acc-nodes dir coord n))
            :else (recur (inc n) acc-tiles acc-nodes)))))

(defn ^:private i-from [coord size]
  (+ (* (second coord) size) (first coord)))

(defn ^:private lookup-closest [found tiles size nodes id target]
  (let [node (first nodes)
        coord (:coord node)
        i (i-from coord size)
        dir (:direction node)]
    (cond (target-aqquired? found target)
            found
          (closest-mine-located? found tiles i id)
            (recur (assoc found :mine dir) tiles size (vec (rest nodes)) id target)
          (closest-tavern-located? found tiles i)
              (recur (assoc found :tavern dir) tiles size (vec (rest nodes)) id target)
            (enemy-located? found tiles i id)
              (recur (assoc found (enemy tiles i) dir) tiles size nodes id target)
          (tavern-or-mine? tiles i)
            (recur found tiles size (vec (rest nodes)) id target)
          :else
          (let [visited (explore-neighbouring-nodes tiles size nodes coord i dir)]
            (recur found (:tiles visited) size (:nodes visited) id target)))))

(defn ^:private back-away [tiles size enemy coord]
  (let [i (i-from coord size)
        shift (partial shift-i-to-direction i size)
        direction (atom (first enemy))]
    (dotimes [n 4]
      (if (and (direction-ok-for-escaping? coord size shift tiles n)
               (not= (first enemy) (label n)))
        (swap! direction (fn [x] (label n)))))
    (println "Escaping to", @direction)
    @direction))

(defn ^:private take-action [action input target]
  (let [board (:board (:game input))
        size (:size board)
        tiles (:tiles board)
        coord (:pos (:hero input))
        id (:id (:hero input))]
    (if (= :search action)
      (lookup-closest {} tiles size [{:direction [] :coord coord}] id target)
      (back-away tiles size target coord))))

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
