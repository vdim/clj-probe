;; Counting some statistic from file with developer's working hours, e.g.
;; sum of hours and etc.

(ns name.vdim.whcount
  [:use clojure.contrib.str-utils 
   [clojure.contrib.duck-streams :only (reader)]])

(defn create-cal
  "Creates instance of java.util.Calendar from specified date string
  by specified date format."
  [date-string date-format]
  (let [date (. (java.text.SimpleDateFormat. date-format) parse date-string (java.text.ParsePosition. 0))]
        (doto (java.util.Calendar/getInstance)  (.setTime date))))

(defn get-calendar-value 
  "Returns specified value (month, day, year and so on) from 
  specified date string and specified date format"
  [date-string date-format field]
  (. (create-cal date-string date-format) get field))

(defn get-hours
  "Returns number representation of hours.
  If param hours is not valid then 0 is returned."
  [hours]
  (try 
    (Double/parseDouble hours) 
    (catch NumberFormatException _ 0)))

(defn count-hours-on-value
  "First param is path to file with working hours. 
  Second param is function that gets vector of string (getting from each line of file) 
  and returns necessary key. (For example, let's suppose that line from file 
  is \"19.07.2011 CO 1 Improved whcount library.\". So func gets vector 
  [\"19.07.2011\" \"CO\" \"1\" \"Imroved\" \"whcount\" \"library.\"]. If we counts hours
  by month then func should returns 7.)

  Counts working hours distributed by some value 
  (for example, month or activity). Returns map where key is value and value is hours for this value."
  [filename func] 
  (reduce 
    #(if (or (empty? %2) (= \# (first %2)))
       %1
       (let [vec-wh (vec (re-split #"\s+" %2)),
             year (get-calendar-value (vec-wh 0) "dd.MM.yyyy" (java.util.Calendar/YEAR))
             key-value (func vec-wh),
             map-for-year (get %1 year),
             old-value (let [v (get map-for-year  key-value)] (if (nil? v) 0 v))]
         (assoc %1 year (assoc map-for-year key-value (+ old-value (get-hours (vec-wh 2)))))))
    {}
    (line-seq (reader filename))))


(defn get-hours-on-week 
  "List where value is map which is distribution hours on week 
  (key is number of week, value is hours in this week). 
  For example: {{1 10 2 20 3 30} {1 5 2 25 3 25}}"
  [filename]
  (count-hours-on-value 
          filename 
          #(get-calendar-value (%1 0) "dd.MM.yyyy" (java.util.Calendar/WEEK_OF_YEAR))))

(defn get-marks-on-week 
  "Gets list with distribution hours on week and returns mark of each week."
  [flatten-hours-on-week]
  (map 
    #(cond (< %1 30) :full-fail 
           (< %1 40) :fail
           (< %1 50) :so-so
           :else :success) 
    flatten-hours-on-week))

(defn count-marks
  "Retursn a number of specified mark in marks-on-week.
  For example, (count-marks :success '(:fail :success :full-fail :fail)) 
  returns 1"
  [mark, marks-on-week]
  (reduce #(if (= %2 mark) (+ %1 1) %1) 0 marks-on-week))

(def month-names
  ^{:doc "Pretty names of month."}
  {0 "January" 
   1 "February" 
   2 "March" 
   3 "April" 
   4 "May" 
   5 "June" 
   6 "July" 
   7 "August" 
   8 "September" 
   9 "October" 
   10 "November" 
   11 "December"})

(def activity-names
  ^{:doc "Pretty names of activity."}
  {"ME" "Meetings (ME)" 
   "LC" "Other kind of conversation (LC)" 
   "PP" "Project plan (PP)" 
   "PR" "Projection (PR)" 
   "DO" "Documentations (DO)" 
   "CO" "Coding (include debugging) (CO)" 
   "TE" "Testing (TE)" 
   "AD" "Administration tasks (AD)" 
   "TM" "Supporting (TM)" 
   "RE" "Specification of requirements (RE)" 
   "RD" "Reading documentation (RD)" 
   "OT" "Other (OT)"})

(defn pprint-map
  "Pretty prints map. First param is user map.
  Second param is map where key is key from user map 
  and value is pretty string representation of key."
  [user-map pretty-name-keys]
  (reduce
    #(str %1 "\n\t" (pretty-name-keys (%2 0)) " = " (%2 1))
    "" 
    user-map))

(defn pprint-year-hours
  "Pretty print map with years and hours which distributed by some value"
  [year-hours pretty-name-keys] 
  (reduce #(str %1 "\n[" (%2 0) "]" (pprint-map (%2 1) pretty-name-keys)) "" year-hours))


(defn pprint-marks
  "Pretty prints marks of week."
  [hours-on-week]
  (str "Marks:"
       (reduce (fn [s [k v]] 
                 (let [marks (get-marks-on-week (vals v))]
                   (str s "\n[" k "]" \newline
                        "\tCount of full fail weeks: " (count-marks :full-fail marks) \newline 
                        "\tCount of fail weeks: " (count-marks :fail marks) \newline
                        "\tCount of so-so weeks: " (count-marks :so-so marks) \newline
                        "\tCount of success weeks: " (count-marks :success marks)))) 
               "" hours-on-week)))


(defn get-stats [filename]
  (let [hours-on-week (get-hours-on-week filename)
        flatten-hours-on-week (reduce #(flatten (cons %1 (vals %2))) () (vals hours-on-week))
        marks-on-week (get-marks-on-week flatten-hours-on-week)
        weeks (count flatten-hours-on-week)
        hours (apply + flatten-hours-on-week)]
    (println "Total hours: ", hours)
    (println "Hours by year: " (reduce (fn [m [k v]] (assoc m k (apply + (vals v))))
                                       {} hours-on-week))
    (println "Count of weeks: " weeks)
    (println "Hours on weeks: " flatten-hours-on-week)
    (println "Sorted hours on weeks: " (sort flatten-hours-on-week))
    (println (pprint-marks hours-on-week))
    (println "Average hours on week: ", (/ (double hours) weeks))
    (println "Max on week: ", (apply max flatten-hours-on-week))
    (println "Min on week: ", (apply min flatten-hours-on-week))
    (println "Hours on month: ", 
            (pprint-year-hours (count-hours-on-value 
                          filename 
                          #(get-calendar-value (%1 0) "dd.MM.yyyy" (java.util.Calendar/MONTH))) 
                          month-names))
    (println "Hours on activity: ", (pprint-year-hours (count-hours-on-value filename #(%1 1)) activity-names))))

