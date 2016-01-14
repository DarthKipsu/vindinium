(ns vindinium.pathfinder-test
  (:require [clojure.test :refer :all]
            [vindinium.pathfinder :refer :all]
            [midje.util :refer [expose-testables]])
  (:use midje.sweet))

(expose-testables vindinium.pathfinder)

(def test-tiles [{:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall}
                 {:tile :wall} {:tile :hero :id 1} {:tile :mine :of 1} {:tile :air} {:tile :mine} {:tile :hero :id 2} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :tavern} {:tile :wall :visited true} {:tile :tavern} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :hero :id 3} {:tile :mine} {:tile :air} {:tile :mine} {:tile :hero :id 4} {:tile :wall}
                 {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall}])

(facts "pathfinder knows when objects are found"
  (fact "can tell if target is aquired"
    (target-aqquired? {:mine ["south"]} :mine) => true
    (target-aqquired? {:mine ["south"] :tavern ["west"]} :tavern) => true
    (target-aqquired? {:mine ["south"]} :tavern) => false
    (target-aqquired? {} :mine) => false)

  (fact "can tell if closest mine is located"
    (closest-mine-located? {} test-tiles 11 1) => true
    (closest-mine-located? {} test-tiles 10 1) => false)
  (fact "closest mine not located if one is already found"
    (closest-mine-located? {:mine ["east"]} test-tiles 11 1) => false)
  (fact "closest mine not located if it's already yours"
    (closest-mine-located? {} test-tiles 9 1) => false)

  (fact "can tell if closest tavern is located"
    (closest-tavern-located? {} test-tiles 23) => true
    (closest-tavern-located? {} test-tiles 22) => false)
  (fact "closest tavern not located if one is already found"
    (closest-tavern-located? {:tavern ["south"]} test-tiles 23) => false)

  (fact "can tell if enemy is located"
    (enemy-located? {} test-tiles 12 1) => true
    (enemy-located? {} test-tiles 11 1) => false)
  (fact "enemy not located if the same one is already found earlier"
    (enemy-located? {:enemy2 ["south"]} test-tiles 12 1) => false)
  (fact "enemy located when another different enemy is already found"
    (enemy-located? {:enemy3 ["south"]} test-tiles 12 1) => true)
  (fact "enemy not located if its yourself"
    (enemy-located? {} test-tiles 12 2) => false)

  (fact "knows if a tile is a tavern or a mine"
    (tavern-or-mine? test-tiles 9) => true
    (tavern-or-mine? test-tiles 23) => true
    (tavern-or-mine? test-tiles 8) => false))

(facts "cardinal directions are handled correctly"
  (fact "directions are correctly labeled"
    (label 0) => "north"
    (label 1) => "west"
    (label 2) => "south"
    (label 3) => "east")
  (fact "tile index moves to correct directions"
    (north 16 7) => 9
    (west 16) => 15
    (south 16 7) => 23
    (east 16) => 17)
  (fact "moving index with shifting moves to correct direction"
    (shift-i-to-direction 16 7 0) => 9
    (shift-i-to-direction 16 7 1) => 15
    (shift-i-to-direction 16 7 2) => 23
    (shift-i-to-direction 16 7 3) => 17))

(facts "tile propertis and border cases checked correctly"
  (fact "knows when all directions are explored based on current direction"
    (all-directions-explored? 4) => true
    (all-directions-explored? -1) => true
    (all-directions-explored? 3) => false
    (all-directions-explored? 0) => false)
  (fact "can tell if a tile is inside the borders of the game grid"
    (tile-inside-borders? test-tiles -7 0) => false
    (tile-inside-borders? test-tiles -1 0) => false
    (tile-inside-borders? test-tiles 7 0) => true
    (tile-inside-borders? test-tiles 2 0) => true
    (tile-inside-borders? test-tiles 41 48) => true
    (tile-inside-borders? test-tiles 47 48) => true
    (tile-inside-borders? test-tiles 55 48) => false
    (tile-inside-borders? test-tiles 49 48) => false
    (tile-inside-borders? test-tiles 6 7) => false
    (tile-inside-borders? test-tiles 7 6) => false)
  (fact "knows if a tile is visited previously or not"
    (tile-not-previously-visited? test-tiles 24) => false
    (tile-not-previously-visited? test-tiles 23) => true)
  (fact "can tell if a tile does not have a listed feature"
    (tile-not-a-wall? test-tiles 0) => false
    (tile-not-a-wall? test-tiles 8) => true
    (tile-not-a-mine? test-tiles 9) => false
    (tile-not-a-mine? test-tiles 10) => true
    (tile-not-a-hero? test-tiles 12) => false
    (tile-not-a-hero? test-tiles 13) => true))

(facts "checks to see if a move is viable"
  (fact "can tell if a direction is ok for exploring"
    (direction-ok-for-exploring? test-tiles 1 8) => false
    (direction-ok-for-exploring? test-tiles 7 8) => false
    (direction-ok-for-exploring? test-tiles 15 8) => true
    (direction-ok-for-exploring? test-tiles 9 8) => true
    (direction-ok-for-exploring? test-tiles 23 16) => true
    (direction-ok-for-exploring? test-tiles 12 19) => true)
  (fact "can tell if a direction is ok for escaping"
    (direction-ok-for-escaping? test-tiles 1 8) => false
    (direction-ok-for-escaping? test-tiles 7 8) => false
    (direction-ok-for-escaping? test-tiles 15 8) => true
    (direction-ok-for-escaping? test-tiles 9 8) => false
    (direction-ok-for-escaping? test-tiles 23 16) => true
    (direction-ok-for-escaping? test-tiles 12 19) => false))

(def test-tiles-visited-15 [{:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall}
                 {:tile :wall} {:tile :hero :id 1} {:tile :mine :of 1} {:tile :air} {:tile :mine} {:tile :hero :id 2} {:tile :wall}
                 {:tile :wall} {:tile :air :visited true} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :tavern} {:tile :wall :visited true} {:tile :tavern} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :hero :id 3} {:tile :mine} {:tile :air} {:tile :mine} {:tile :hero :id 4} {:tile :wall}
                 {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall}])

(facts "can modify temporary structures to keep track of route"
  (fact "marks a tile visited after a visit to avoid walking in circles"
    (visited test-tiles 15) => test-tiles-visited-15)
  (fact "adds new tiles to queue to be visited later"
    (queue-with-new-node [] [] 2 15) => [{:path ["south"] :i 15}]
    (queue-with-new-node [{:path ["south"] :i 15}] [] 3 16)
        => [{:path ["south"] :i 15} {:path ["east"] :i 16}]
    (queue-with-new-node [] ["south"] 3 9) => [{:path ["south" "east"] :i 9}])
  (fact "turns coordinates to map index"
    (i-from [2 1] 7) => 9
    (i-from [0 0] 7) => 0
    (i-from [6 6] 7) => 48))
