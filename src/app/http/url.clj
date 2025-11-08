(ns app.http.url
  (:require
   [clojure.string :as str]))

(defn encode-uri-component [s]
  (java.net.URLEncoder/encode s))

(defn ->query-string [m]
  (str/join
   "&"
   (mapv
    (fn [[k v]]
      (str (encode-uri-component (name k))
           "="
           (encode-uri-component (str v))))
    m)))

(defn ok-response? [response]
  (<= 200 (:status response) 299))

(defn- decompress [stream content-encoding]
  (case content-encoding
    "gzip" (java.util.zip.GZIPInputStream. stream)
    stream))

(defn- get-stream [conn content-encoding]
  (decompress (or (.getErrorStream conn) (.getInputStream conn)) content-encoding))

(defn- enrich-with-body [response conn opts]
  (assoc
   response
   :body
   (let [content-encoding (get-in response [:headers "content-encoding"])]
     (case (:as opts)
       :stream (get-stream conn content-encoding)
       (with-open [stream (get-stream conn content-encoding)]
         (String. (.readAllBytes stream) java.nio.charset.StandardCharsets/UTF_8))))))

(defn request
  "opts
    required:
      :url - string

    optional:
      :method -  GET | POST | HEAD | OPTIONS | PUT | DELETE
      :headers - header map
      :body - string
      :as - :stream
      :throw-on-error - boolean"
  [opts]
  (let [url (let [u (:url opts)] (if (string? u) (java.net.URL. u) u))
        conn (.openConnection url)]
    (.setRequestMethod conn (:method opts "GET"))
    (doseq [[k v] (:headers opts)]
      (.setRequestProperty conn k v))

    (when-let [body (:body opts)]
      (.setDoOutput conn true)
      (with-open [os (.getOutputStream conn)]
        (with-open [osw (java.io.OutputStreamWriter. os "UTF-8")]
          (.write osw body)
          (.flush osw))))

    (.connect conn)

    (let [status (.getResponseCode conn)
          response (-> {:status status
                        :headers (into {}
                                       (keep (fn [[k v]]
                                               (when k
                                                 [(str/lower-case k) (str/join "," v)])))
                                       (.getHeaderFields conn))}
                       (enrich-with-body conn opts))]
      (when (and (:throw-on-error opts)
                 (not (ok-response? response)))
        (throw (ex-info "Request failed" {:response response})))
      response)))
