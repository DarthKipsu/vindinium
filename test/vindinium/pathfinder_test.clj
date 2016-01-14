(ns vindinium.pathfinder-test
  (:require [clojure.test :refer :all]
            [vindinium.pathfinder :refer :all]
            [midje.util :refer [expose-testables]])
  (:use midje.sweet))

(expose-testables vindinium.pathfinder)

(def test-tiles [{:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall} {:tile :wall}
                 {:tile :wall} {:tile :hero :id 1} {:tile :mine :of 1} {:tile :air} {:tile :mine} {:tile :hero :id 2} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :air} {:tile :wall}
                 {:tile :wall} {:tile :air} {:tile :tavern} {:tile :wall} {:tile :tavern} {:tile :air} {:tile :wall}
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
