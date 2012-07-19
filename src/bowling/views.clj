(ns bowling.views
  (:require [clojure.set :as sets]
	    [clj-time.format :as df]
	    [clj-time.core :as time]
	    [clj-time.coerce :as tc]
	    [korma.core :as db])
  (:use [bowling.time_helpers :only [next-day]]
	[bowling.models :only [games insert-game]]
	[bowling.charts :only [time-scatter-plot]]
	[incanter.core :only [to-dataset]]
	[seesaw core mig table]
	[seesaw.color]
	[seesaw.style])
  (:import org.jfree.chart.ChartPanel))

;; Make things look a little bit nicer.
(native!)
(javax.swing.UIManager/setLookAndFeel 
 "org.pushingpixels.substance.api.skin.SubstanceTwilightLookAndFeel")

(defn date-to-string [date]
  (df/unparse (df/formatters :year-month-day) date))

(defn config-get
  "Wrapper for config. Provides the option of getting a default for when a field
  is not set."
  ([widget field] (config widget field))
  ([widget field default] (if-let [ret (config widget field)] ret default)))

(defn clear-field [field]
  (config! field :text "")
  (config! field :class #{}))

(defn date-validator? [text-field]
  "Returns the date in the text-field if there is one, otherwise nil."
  (try (df/parse (df/formatters :year-month-day) (value text-field))
       (catch IllegalArgumentException e nil)))


(invoke-later
 (def styles {[:JTextField] {:background (.getBackground (text))}
	      [:JTextField.validation-error] {:background :red}}))

(defn create-chart [date-from date-to]
  (doto (time-scatter-plot
	 :chart-date :pins
	 :title (str "Scores from " (date-to-string date-from) " to " 
		     (date-to-string date-to))
	 :x-label "Days"
	 :y-label "Pins"
	 :data (to-dataset 
		(db/select games (db/where
				  (and (<= date-from :date)
				       (<= :date date-to))))))
    (-> .getPlot
	.getRangeAxis
	(.setRange 0 300))
    (-> .getPlot
	.getDomainAxis
	(.setRange (org.jfree.data.time.DateRange. (tc/to-date date-from)
						   (tc/to-date date-to))))))

(defn create-panel
  ([chart & options]
     (let [opts (when options (apply assoc {} options))
	   window-title (or (:window-title opts) "Incanter Plot")
	   width (or (:width opts) 500)
	   height (or (:height opts) 400)
	   panel (ChartPanel. chart)]
       (doto panel
	 (.setSize width height))
       panel)))

(def default-from-date (time/plus (next-day (time/now)) (time/days -7)))
(def default-to-date (next-day (time/now)))


     
(invoke-later
(def root (frame :title "Bowling Score Tracker"))
(def date-range-form
  (let [date-from-field (text :id :date-from
			      :text (date-to-string default-from-date)
			      :columns 10
			      :tip "YYYY-MM-DD")
	date-to-field (text :id :date-to
			    :text (date-to-string default-to-date)
			    :columns 10
			    :tip "YYYY-MM-DD")
	date-range-button (button :text "Render")
	_ (listen (.getDocument date-to-field)
	    :document (fn [e]
			(if-let [date (date-validator? date-to-field)]
			  (config! date-to-field :class "")
			  (config! date-to-field :class "validation-error"))
			(apply-stylesheet root styles)))
	_ (listen (.getDocument date-from-field)
	    :document (fn [e]
			(if-let [date (date-validator? date-from-field)]
			  (config! date-from-field :class "")
			  (config! date-from-field :class "validation-error"))
			(apply-stylesheet root styles)))
	_ (listen date-range-button
	    :action (fn [e]
		      (try (.setChart (first (select root [:ChartPanel]))
				      (create-chart
				       (date-validator? date-from-field)
				       (date-validator? date-to-field)))
			   (catch Exception e (alert "Invalid date range.")))))
	date-range-form (horizontal-panel
			 :items [date-from-field 
				 date-to-field
				 date-range-button])]
    date-range-form)))


(invoke-later
(def scores-chart-panel
     (border-panel
      :id :scores_panel
      :north (create-panel
	      (create-chart default-from-date default-to-date))
      :south date-range-form)))

;; Initialization
(invoke-later
(def recently-added-table (table :model [:columns [:date :game :pins]]))
(def add-game-date-field (text :id :date :columns 10 
			       :text (date-to-string (time/now))))
(def add-game-score-field (text :id :pins :columns 10))
(def add-game-button (button :text "Add Game"))

(def add-game-panel
     (mig-panel :constraints ["" "" ""]
		:items [["Date" "r"] 
			[add-game-date-field "wrap"]
			["Score" "r"]
			[add-game-score-field "wrap"]
			[add-game-button "span, wrap"]
			["Recently Added Scores" "span, align center, wrap"]
			[(scrollable recently-added-table :size [280 :by 82]) 
			 "span, align center"]]))

(defn score-validator? [text-field]
  (try (<= 0 (Integer/parseInt (value text-field)) 300)
       (catch NumberFormatException e nil)))

(defn enable-add-game-button []
  (config! add-game-button :enabled? 
	   (and (date-validator? add-game-date-field) 
		(score-validator? add-game-score-field))))

(defn repopulate-recently-added-table []
  (clear! recently-added-table)
  (doseq [game (reverse
		(map #(assoc % :date (date-to-string (:date %)))
		     (db/select games 
				(db/order :created_on :DESC) 
				(db/limit 3))))]
    (insert-at! recently-added-table 0 game)))

;; Setup
(repopulate-recently-added-table)
(enable-add-game-button)

(defn form-handler [field is-valid?]
  (fn [e]
    (let [classes (config-get field :class #{})]
      (config! field :class 
	       (if (is-valid? field)
		 (disj classes "validation-error")
		 (conj classes "validation-error"))))
    (apply-stylesheet add-game-panel styles)
    (enable-add-game-button)))

(listen (.getDocument add-game-date-field)
  :document (form-handler add-game-date-field date-validator?))

(listen (.getDocument add-game-score-field)
  :document (form-handler add-game-score-field score-validator?))

(defn add-game-handler [e]
  (let [date (df/parse (df/formatters :year-month-day) 
		       (value add-game-date-field))
	pins (Integer/parseInt (value add-game-score-field))]
    (clear-field add-game-score-field)
    (enable-add-game-button)
    (apply-stylesheet add-game-panel styles)
    (insert-game {:date date :pins pins})
    (repopulate-recently-added-table)))

(listen add-game-button
  :action add-game-handler))

(defn start-app [& options]
  (invoke-later
   (config! root :content 
	    (mig-panel :constraints ["ins 5" "" ""]
		       :items [[add-game-panel "top"]
			       [scores-chart-panel "wrap"]]))
   (when options 
     (doseq [option (apply hash-map options)]
       (config! root (first option) (second option))))
   (-> root show! pack!)))
