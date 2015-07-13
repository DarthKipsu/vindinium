(ns vindinium.pathfinder)

(defn ^:private all-targets-aqquired? [found]
  (< 1 (count found)))

(defn ^:private closest-mine-located? [found tiles i]
  (and (not (:mine found))
       (= :mine (:tile (get tiles i)))
       (not= 1 (:of (:tile (get tiles i))))))

(defn ^:private closest-tavern-located? [found tiles i]
  (and (not (:tavern found))
       (= :tavern (:tile (get tiles i)))))

(defn ^:private label [index]
  (let [labels ["north" "west" "south" "east"]]
    (get labels index)))

(defn ^:private i-shift-to-label-directions [i size n]
  (get [(- i size) (dec i) (+ i size) (inc i)] n))

(defn ^:private all-directions-explored? [n]
  (= n 4))

(defn ^:private tile-inside-borders? [x y size n]
  (get [(pos? y) (pos? x) (< y (dec size)) (< x (dec size))] n))

(defn ^:private tile-not-previously-visited? [tiles index]
  (not (:visited (get tiles index))))

(defn ^:private tile-not-a-wall? [tiles index]
  (not= :wall (:tile (get tiles index))))

(defn ^:private direction-ok-for-exploring? [x y size shift tiles n]
  (and (tile-inside-borders? x y size n)
       (tile-not-previously-visited? tiles (shift n))
       (tile-not-a-wall? tiles (shift n))))

(defn ^:private mark-tile-as-visited [tiles index]
  (assoc-in tiles [index :visited] true))

(defn ^:private shift-coordinates [x y n]
  (get [[x (dec y)] [(dec x) y] [x (inc y)] [(inc x) y]] n))

(defn ^:private add-node-to-queue [nodes direction x y n]
  (let [new-coords (partial shift-coordinates x y)]
    (conj nodes {:direction (conj direction (label n))
                 :coord (new-coords n)})))

(defn ^:private explore-neighbouring-nodes [tiles size nodes x y i dir]
  (let [shift (partial i-shift-to-label-directions i size)]
    (loop [n 0
           acc-tiles tiles
           acc-nodes (vec (rest nodes))]
      (cond (all-directions-explored? n)
              {:tiles acc-tiles :nodes acc-nodes}
            (direction-ok-for-exploring? x y size shift acc-tiles n)
              (recur (inc n)
                     (mark-tile-as-visited acc-tiles (shift n))
                     (add-node-to-queue acc-nodes dir x y n))
            :else (recur (inc n) acc-tiles acc-nodes)))))

(defn ^:private lookup-closest [found tiles size nodes]
  (let [node (first nodes)
        x (first (:coord node))
        y (second (:coord node))
        i (+ (* y size) x)
        dir (:direction node)]
    (cond (all-targets-aqquired? found)
            found
          (closest-mine-located? found tiles i)
            (recur (assoc found :mine dir) tiles size nodes)
          (closest-tavern-located? found tiles i)
            (recur (assoc found :tavern dir) tiles size nodes)
          :else
          (let [visited (explore-neighbouring-nodes tiles size nodes x y i dir)]
            (recur found (:tiles visited) size (:nodes visited))))))

(defn breath-first-search [board start-pos]
  (let [size (:size board)
        tiles (:tiles board)]
    (lookup-closest {} tiles size [{:direction [] :coord start-pos}])))
