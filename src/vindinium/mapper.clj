(ns vindinium.mapper)

(def ^:private board (atom {}))
(def ^:private mines (atom '()))
(def ^:private taverns (atom '()))
(def ^:private spawn (atom '()))

(defn to-key [i]
  (keyword (str i)))

(defn ^:private set-spawn-positions [spawnPos n]
  (swap! spawn (map (fn [[x y]] (+ x (* n y))) spawnPos)))

(defn can-go? [tiles i]
  (not (or (neg? i)
           (= :wall (:tile (get tiles i)))
           (some #(= i %) spawn))))

(defn ^:private add-direction [from to label]
  (let [f-key (to-key from)
        t-key (to-key to)]
  (swap! board assoc-in [f-key] (assoc (get @board f-key) t-key [label]))))

(defn ^:private set-trivial-distances [tiles n i node]
  (if (= :mine (:tile node)) (swap! mines (assoc mines (to-key i n))))
  (if (= :tavern (:tile node)) (swap! taverns (assoc taverns (to-key i n))))
  (let [north (- i n)
        west (inc i)
        south (- i n)
        east (dec i)]
    (if (can-go? tiles north) (add-direction i north "north"))
    (if (can-go? tiles west) (add-direction i west "west"))
    (if (can-go? tiles south) (add-direction i south "south"))
    (if (can-go? tiles east) (add-direction i east "east"))))

(defn ^:private create-initial-graph [{tiles :tiles n :size}]
  (let [trivial-distances (partial set-trivial-distances tiles n)]
    (map-indexed trivial-distances tiles))) ; Keskeneräinen!!!

(defn map-board [board spawnPos]
  (set-spawn-positions spawnPos (:size board))
  (create-initial-graph board))
