(ns vindinium.core
  (:gen-class)
  (:use [vindinium.debug_help :only [print-board-from-input]]
        [vindinium.pathfinder :only [breath-first-search]]
        [slingshot.slingshot :only [try+, throw+]]
        [clojure.core.match :only [match]]))

(require '[clj-http.client :as http])

(def server-url "http://vindinium.org")

(defn bot [input]
  "Implement this function to create your bot!"
  (let [closest (partial breath-first-search
                         (:board (:game input))
                         (:pos (:hero input))
                         (:id (:hero input)))]
    (if (> 40 (:life (:hero input)))
      (first (:tavern (closest :tavern)))
      (first (:mine (closest :mine))))))

(defn at [[x y] tiles size]
  (tiles (+ (* y size) x)))

; Because the (y,x) position of the server is inversed. We fix it to (x,y).
(defn fix-pos [{:keys [x y]}] [y x])

(defn fix-hero [hero]
  (-> hero
    (update-in [:pos] fix-pos)
    (update-in [:spawnPos] fix-pos)))

(defn improve-input [input]
  (-> input
    (update-in [:hero] fix-hero)
    (update-in [:game :heroes] #(map fix-hero %))
    (update-in [:game :board :tiles] vec)))

(defn parse-tile [tile]
  (match (vec tile)
         [\space \space] {:tile :air}
         [\# \#] {:tile :wall}
         [\[ \]] {:tile :tavern}
         [\$ \-] {:tile :mine}
         [\$ i] {:tile :mine :of (Integer/parseInt (str i))}
         [\@ i] {:tile :hero :id (Integer/parseInt (str i))}))

(defn parse-tiles [tiles] (map parse-tile (partition 2 (seq tiles))))

(defn parse-input [input] (update-in input [:game :board :tiles] parse-tiles))

(defn request [url, params]
  "makes a POST request and returns a parsed input"
  (try+
    (-> (http/post url {:form-params params :as :json})
      :body
      parse-input
      improve-input)
    (catch map? {:keys [status body]}
      (println (str "[" status "] " body))
      (throw+))))

(defn step [from]
  (loop [input from]
    ;(print-board-from-input input)
    (let [next (request (:playUrl input) {:dir (bot input)})]
      (if (:finished (:game next)) (println "") (recur next)))))

(defn training-real [secret-key turns]
  (let [input (request (str server-url "/api/training") {:key secret-key :turns turns})]
    (println (str "Starting training game " (:viewUrl input)))
    (step input)
    (println input)
    (println (str "Finished training game " (:viewUrl input)))))

(defn arena-real [secret-key games]
  (loop [it 1]
    (let [p #(println (str "[" it "/" games "] " %))
          _ (p "Waiting for pairing...")
          input (request (str server-url "/api/arena") {:key secret-key})]
      (p (str "Starting arena game " (:viewUrl input)))
      (step input)
      (p (str "Finished arena game " (:viewUrl input)))
      (when (< it games) (recur (+ it 1))))))

(defn secret-key []
  (if (.exists (clojure.java.io/as-file "secretkey"))
    (clojure.string/trim (slurp "secretkey"))
    (throw (Exception. "Missing secret key! Please add your secret key to file named secretkey."))))

(def training (partial training-real (secret-key)))
(def arena (partial arena-real (secret-key)))
 

(def usage
  "Usage:
  training <number-of-turns>
  arena <number-of-games")

(defn -main [& args]
  (match (vec args)
         ["training", nb] (training nb)
         ["arena", nb] (arena nb)
         :else (.println *err* usage)))

