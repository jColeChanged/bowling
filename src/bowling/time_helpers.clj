(ns bowling.time_helpers
  (:require [clj-time.core :as dt]))

(defn start-of-day [datetime]
  "Returns a new datetime that is the start of the day."
  (dt/date-time (dt/year datetime) (dt/month datetime) (dt/day datetime)))

(defn next-day [datetime]
  "Returns a new datetime that is the start of the next day."
  (dt/plus (start-of-day datetime) (dt/hours 24)))