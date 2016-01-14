(ns vindinium.debug_help
  (:use [clojure.core.match :only [match]]))

(defn ^:private unparse-tile [tile]
  (match (:tile tile)
         :air "  "
         :wall "##"
         :tavern "[]"
         :mine (if (:of tile) (str "$" (:of tile)) "$-")
         :hero (str "@" (:id tile))))

(defn ^:private print-board [board-size tiles row column]
  (if (= row board-size)
    (println "")
    (do (if (zero? column) (println ""))
        (print (unparse-tile (first tiles)))
        (recur board-size
               (rest tiles)
               (if (= board-size (inc column)) (inc row) row)
               (if (= board-size (inc column)) 0 (inc column))))))

(defn print-board-from-input
  "Prints the game board in it's true form for each iteration of a game."
  [input]
  (let [board (:board (:game input))
        board-size (:size board)
        tiles (:tiles board)]
    (print-board board-size tiles 0 0)))
