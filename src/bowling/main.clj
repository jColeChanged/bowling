(ns bowling.main
  (:use [bowling.views :only [start-app]])
  (:use [bowling.models :only [create-game-table]])
  (:gen-class))

(defn -main [& rest]
  (create-game-table)
  (start-app :on-close :exit))