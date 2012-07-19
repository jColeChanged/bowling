(ns bowling.charts
  (:use [incanter.core])
  (:use [incanter.charts])
  (:require [clj-time.core :as time])
  (:require [clj-time.coerce :as coerce-time]))

;; Create some fake data to play with

(defn rand-date []
  (time/plus (time/now) (time/days (rand-int 10))))

(defn create-game-record []
  (let [date (rand-date)]
    [(rand-int 301) date (coerce-time/to-long date)]))

(def games (repeatedly 10 create-game-record))

;; Now that we have some games lets try to put those games in a dataset...

(def games-dataset (dataset [:pins :date :ms] games))

;; Now that we have some data lets seeif we can display what we have in a chart

;; This takes the dataset and returns a different dataset which has been sorted
;; should be pretty clear what is happening...

(def chart (scatter-plot :pins :ms :data games-dataset))

;; (view (time-series-plot :ms :pins 
;; 			:title   "Scores for period"
;; 			:y-label "Score"
;; 			:x-label "Days"
;; 			:data games-dataset))


(import 'org.jfree.data.xy.XYSeries)
(import 'org.jfree.chart.axis.DateAxis)
(import 'org.jfree.chart.axis.DateTickUnit)
(import 'org.jfree.chart.axis.NumberAxis)
(import 'org.jfree.data.xy.XYSeriesCollection)

(def data-series (XYSeries. "Game scores from x to y"))
(def date-axis (DateAxis. "Days"))
(def unit (DateTickUnit. DateTickUnit/DAY 1))
;; (.setTickUnit date-axis unit)
(def num-axis (NumberAxis. "Pins"))
(def renderer (org.jfree.chart.renderer.xy.XYBlockRenderer.))
(def xy-dataset (XYSeriesCollection. data-series))
(def plot (org.jfree.chart.plot.XYPlot. xy-dataset date-axis num-axis renderer))

(defn scatter-plot**
  ([x y & options]
    (let [opts (when options (apply assoc {} options))
          data (:data opts)
          _x (if (coll? x) (to-list x) ($ x data))
          _y (if (coll? y) (to-list y) ($ y data))
          _group-by (when (:group-by opts)
                      (if (coll? (:group-by opts))
                        (to-list (:group-by opts))
                        ($ (:group-by opts) data)))
          x-groups (when _group-by
                     (map #($ :col-0 %)
                          (vals ($group-by :col-1 (conj-cols _x _group-by)))))
          y-groups (when _group-by
                     (map #($ :col-0 %)
                          (vals ($group-by :col-1 (conj-cols _y _group-by)))))
          __x (if x-groups (first x-groups) _x)
          __y (if y-groups (first y-groups) _y)
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (str 'x))
          y-lab (or (:y-label opts) (str 'y))
          series-lab (or (:series-label opts)
                         (if x-groups
                           (format "%s, %s (0)" 'x 'y)
                           (format "%s, %s" 'x 'y)))
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          data-series (XYSeries. series-lab)
          _dataset (XYSeriesCollection.)
          chart (do
                  (dorun
                   (map (fn [x y]
                          (if (and (not (nil? x)) (not (nil? y)))
                            (.add data-series (double x) (double y))))
                        __x __y))
                  (.addSeries _dataset data-series)
                  (org.jfree.chart.ChartFactory/createScatterPlot
                   title
                   x-lab
                   y-lab
                   _dataset
                   org.jfree.chart.plot.PlotOrientation/VERTICAL
                   legend?
                   true	; tooltips
                   false))
	  _ (.setDomainAxis (.getPlot chart) date-axis) 
          _ (when x-groups
              (doseq [i (range 1 (count x-groups))]
                (add-points chart
                            (nth x-groups i)
                            (nth y-groups i)
                            :series-label (format "%s, %s (%s)" 'x 'y i))))]
      (.setSeriesShape (-> chart .getPlot .getRenderer) 0 (java.awt.geom.Ellipse2D$Double. -3 -3 6 6))
      (.setSeriesShape (-> chart .getPlot .getRenderer) 1 (java.awt.geom.Rectangle2D$Double. -3 -3 6 6))
      (set-theme chart theme)
      chart)))

(defn time-scatter-plot [& args]
  (apply scatter-plot** args))

   ;; :ms :pins 
   ;; 		  :x-label "Days" 
   ;; 		  :y-label "Score" 
   ;; 		  :series-label "Game Scores from x to y"
   ;; 		  :data games-dataset))
