(ns app.core
  (:require
   [clojure.java.shell]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [app.util :as u]
   [app.http.url :as http])
  (:import
   [org.jsoup Jsoup]))

(defn sh [& args]
  (let [res (apply clojure.java.shell/sh args)]
    (if (= 0 (:exit res))
      (:out res)
      (throw (ex-info "Error executing shell commend" res)))))

(defn- parse-html*
  ([url]
   (parse-html* nil url))
  ([cookie url]
   (let [response (http/request
                   {:url url
                    :headers (cond-> {}
                               cookie (assoc "Cookie" cookie))
                    :as :stream})]
     (with-open [xin (:body response)]
       (Jsoup/parse xin "utf-8" url)))))

(def ^{:arglists (:arglists (meta #'parse-html*))} parse-html
  (u/retry 3 0 parse-html*))

(defn- image->text [src]
  (sh "sh" "-c" (str "curl --output - --silent " src " | tesseract - stdout")))

(defn- carousel-srcs [doc]
  (into
   []
   (keep (fn [img]
           (let [src (.getValue (.attribute img  "src"))]
             (when (str/starts-with? src "https://")
               src))))
   (.select doc "#carousel img")))

(defn- closest-year [now month day]
  (let [current-year (.getYear now)
        this-year (java.time.LocalDate/of current-year month day)
        prev-year (.withYear this-year (dec current-year))
        next-year (.withYear this-year (inc current-year))
        now-epoch-day (.toEpochDay (.toLocalDate now))
        date-distances (mapv
                        (fn [date]
                          [date (Math/abs (- now-epoch-day (.toEpochDay date)))])
                        [this-year prev-year next-year])]
    (ffirst (sort-by second date-distances))))

(defn- interesting-events [now]
  (into
   []
   (comp
    (mapcat (fn [src]
              (str/split-lines
               (image->text src))))
    (keep (fn [line]
            (when-let [match (re-matches #"(\d+).(\d+). +(.+)" line)]
              (let [[_ d m s] match
                    month (Integer/valueOf m)
                    day (Integer/valueOf d)
                    at-this-year (closest-year now month day)]
                {:date at-this-year
                 :name s}))))
    (filter (fn [m]
              (= (:name m) "Open Play"))))
   (carousel-srcs (parse-html "https://www.herniprostor.cz/"))))

(defn- events->ical-events [now events]
  (mapv
   (fn [{:keys [name date]}]
     (let [date-str (.format date java.time.format.DateTimeFormatter/BASIC_ISO_DATE)]
       [["BEGIN" "VEVENT"]
        ["UID" (str date-str "/" name)]
        ["DTSTAMP" (.format now (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'"))]
        ["DTSTART;VALUE=DATE" date-str]
        ["SUMMARY" name]
        ["END" "VEVENT"]]))
   events))

(defn- create-calendar [ical-events]
  [["BEGIN" "VCALENDAR"]
   ["VERSION" "2.0"]
   ["PRODID" "nenadalm.github.io/calendars/all"]
   (into [] (comp cat) ical-events)
   ["END" "VCALENDAR"]])

(defn- calendar->str [calendar]
  ;; todo: line should be max 75 octets (https://en.wikipedia.org/wiki/ICalendar#Design)
  (str/join
   "\r\n"
   (into
    []
    (comp
     (mapcat (fn [v]
               (cond
                 (vector? (first v)) v
                 (string? (first v)) [v])))
     (map (fn [[k v]] (str k ":" v))))
    calendar)))

(defn- interesting-events-ical []
  (let [now (java.time.LocalDateTime/now (java.time.ZoneId/of "UTC"))
        events (interesting-events now)]
    (-> (events->ical-events now events)
        create-calendar
        calendar->str)))

(defn build-web [_]
  (let [file (io/file "resources/public/all.ics")]
    (-> file
        .getParentFile
        .mkdirs)
    (spit
     file
     (interesting-events-ical))))
