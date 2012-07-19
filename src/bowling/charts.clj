(ns bowling.charts
  (:use [incanter core charts])
  (:require [clj-time.core :as time]
	    [clj-time.coerce :as coerce-time])
  (:import org.jfree.data.xy.XYSeries
	   org.jfree.data.xy.XYSeriesCollection  
	   org.jfree.chart.axis.DateAxis))

(defmethod to-list :default [s]  (if (seq? s) s (list s)))

(def date-axis (DateAxis.))
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