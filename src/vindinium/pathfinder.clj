(ns vindinium.pathfinder)

(def labels [:north :west :south :east])

(defn ^:private all-targets-aqquired? [found]
  (< 1 (count found)))

(defn ^:private closest-mine-located? [found tiles i]
  (and (not (:mine found))
       (= :mine (:tile (get tiles i)))
       (not= 1 (:of (:tile (get tiles i))))))

(defn ^:private closest-tavern-located? [found tiles i]
  (and (not (:tavern found))
       (= :tavern (:tile (get tiles i)))))

(defn ^:private explore-neighbouring-nodes [tiles size nodes x y i dir]
  (let [movements [(- i size) (dec i) (+ i size) (inc i)]
        tile-inside-borders [(pos? y) (pos? x) (< y (dec size)) (< x (dec size))]
        new-coords [[x (dec y)] [(dec x) y] [x (inc y)] [(inc x) y]]]
    (loop [acc-index 0 acc-tiles tiles acc-nodes (vec (rest nodes))]
      (if (= 4 acc-index) {:tiles acc-tiles :nodes acc-nodes}
          (if (and (get tile-inside-borders acc-index)
                   (not (:visited (get acc-tiles (get movements acc-index))))
                   (not= :wall (:tile (get acc-tiles (get movements acc-index)))))
              (recur (inc acc-index)
                     (assoc-in acc-tiles [(get movements acc-index) :visited] true)
                     (conj acc-nodes {:direction (conj dir (name (get labels acc-index)))
                                                   :coord (get new-coords acc-index)}))
              (recur (inc acc-index) acc-tiles acc-nodes))))))

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
