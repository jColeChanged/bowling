(ns bowling.models
  (:require [clojure.java.jdbc :as sql])
  (:require [clj-time.format :as df])
  (:require [clj-time.coerce :as dc])
  (:require [clj-time.core :as dt])
  (:use [korma core db]))

(defn start-of-day [datetime]
  "Returns a new datetime that is the start of the day."
  (dt/date-time (dt/year datetime) (dt/month datetime) (dt/day datetime)))

(defn next-day [datetime]
  "Returns a new datetime that is the start of the next day."
  (dt/plus (start-of-day datetime) (dt/hours 24)))

(defn string-to-date [string]
  "Converts an SQLite datetime into a Joda DateTime instance."
  (df/parse (df/formatters :date-time) string))

;; I want to work with a databse, because it makes sense to use one instead of
;; building a less feature filled database every time I want to make an app that
;; can persist data across sessions. However, I know that someone who uses the
;; program might not have sqlite or whatever database I decide to use. If I do
;; not want to force Bill to download an SQL server and run it when he wants to
;; use the application, what do I do? SQLite solves this problem.

;; I want the location of the databsae to be cross platform while also being 
;; kind of hardcoded. This is the solution I came up with.
(def db-subname (str (System/getProperty "user.dir") "/db.db"))

;; When setting up a database connection you need to pass it information about
;; the database. You also need to have a database on file. In project.clj I
;; include a project which gives me access to the sqlite.JDBC driver. The other
;; specs are those that are needed to work with sqlite.
(def db-specs   {:classname   "org.sqlite.JDBC"
		 :subprotocol "sqlite"
		 :subname      db-subname})

;; So :date is basically having name called on it. I have this as a function
;; because you can't create the table twice.
(defn create-game-table
  "Creates a table of past games."
  []
  (try 
    (sql/with-connection db-specs
      (sql/transaction
       (sql/create-table :games
			 [:id :integer "PRIMARY KEY"]
			 [:created_on :datetime "DEFAULT CURRENT_TIMESTAMP"]
			 [:date :date]
			 [:pins :integer]
			 [:game :integer]
			 [:game_count :integer])))
    (catch Exception e (println e))))

(defdb production db-specs)

;; When displaying charts we want games played on the same date to all be
;; displayed instead of having them all stack up on each other. Having them
;; be spread throughout the day as far as they can be would also be nice, as
;; it would make each point more legible. The implementation leads to a 
;; tendency to report a game as being towards the middle of the day instead
;; of the start and end.
(defn chart-date [record]
  "Accepts a games record. Assumes that the record has not yet been transformed
  and that it is already at the start of the day. Returns a date which has been
  adjusted according to considerations related to charting."
  (let [date (string-to-date (:date record))]
    (dt/plus date (dt/hours (* (/ (:game record) 
				  (inc (:game_count record)))
			       24)))))

(defentity games 
  (transform (fn [row]
	       ;; If it is a game record and not an aggregate then modify the
	       ;; record otherwise return the aggregate.
	       (if (:game row)
		 (assoc row :date (string-to-date (:date row))
			:chart-date (dc/to-long (chart-date row)))
		 row)))
  (prepare (fn [row]
	     ;; To keep bad dates from introducing graphing errors I 
	     ;; coerce datetimes into date objects. Because I don't
	     ;; know ho
	     (if (:date row)
	       (assoc row :date (start-of-day (:date row)))
	       row)))
  (database production))

(defn insert-game [row]
  "Inserts a game and makes sure that game-count is consistent."
  (transaction 
   (let [constrain-same-day (fn [base]
			      (let [datetime (:date row)]
				(-> base
				    (where (and (<= (start-of-day datetime) 
						    :date)
						(< :date (next-day datetime)))))))
	 game-count (:games_count 
		     (first 
		      (select games (aggregate (count :game) :games_count)
			      constrain-same-day)))
	 adjusted-game-count (inc game-count)]
     (update games 
    	    (set-fields {:game_count adjusted-game-count})
    	    constrain-same-day)
     (insert games (values (assoc row 
			     :game adjusted-game-count
			     :game_count adjusted-game-count))))))
 
