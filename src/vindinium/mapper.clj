(ns vindinium.mapper)

(def ^:private board (atom {}))
(def ^:private mines (atom '()))
(def ^:private taverns (atom '()))
(def ^:private spawn (atom '()))

(defn to-key [i n]
  (keyword (str (mod i n) "-" (quot i n)))) ; muuta

(defn ^:private set-spawn-positions [spawnPos n]
  (swap! spawn (map (fn [[x y]] (+ x (* n y))) spawnPos)))

(defn can-go? [tiles i]
  (not (or (neg? i)
           (= :wall (:tile (get tiles i)))
           (some #(= i %) spawn))))

(defn ^:private add-direction [from to label]
  (swap! board (assoc board (to-key from to) [label])))

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


; MUUTA KEYWORDIT INDEXEIKSI!!!!!!!!!!!!!!!!!!
