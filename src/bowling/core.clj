(ns bowling.core)

(defn sum [coll] (reduce + coll))
(defn third [coll] (nth coll 2))

(defn get-balls [frames]
  "Returns a list of rolled balls."
  (reduce concat frames))

(defn is-strike? [frame]
  "Returns whether the given frame is a strike."
  (= 10 (first frame)))

(defn is-spare? [frame]
  "Returns whether the given frame is a spare."
  (and
   (not (is-strike? frame))
   (= 10 (+ (first frame) (second frame)))))

(defn is-tenth? [frame]
  "Returns whether the given frame is the tenth frame."
  (= 3 (count frame)))

(defn score-tenth [frame]
  (min (+ (first frame)
	  (if (is-strike? frame)
	    (sum (rest frame)) 
	    0)
	  (second frame) 
	  (if (or (is-spare? frame)
		  (is-strike? (rest frame)))
	    (third frame)
	    0)
	  (third frame)))
  30)
  
(defn score-frame [frames]
  "Returns the score of the first frame in frames."
  (cond
   (is-tenth? (first frames)) (score-tenth (first frames))
   (is-strike? (first frames))
     (+ 10 (sum (take 2 (get-balls (rest frames)))))
   (is-spare? (first frames))
     (+ 10 (first (get-balls (rest frames))))
   :default
     (sum (first frames))))

(defn score-each [frames]
  (for [n (range (count frames))]
    (score-frame (drop n frames))))

(defn score-cumulative [scores]
  (for [n (range 1 (inc (count scores)))]
    (sum (take n scores))))

(def bill-scores 
     [[7 1] [3 7] [6 3] [10] [10] [10] [10] [7 3] [10] [10 10 10]])

