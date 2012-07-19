(ns bowling.main
  (:use [bowling.views :only [start-app]])
  (:gen-class))

(defn -main [& rest]
  (start-app :on-close :exit))