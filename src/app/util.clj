(ns app.util)

(defn retry
  "Returns function with same arguments as `f` that if throws is retried at most `n` times with delay of `ms` ms."
  [n ms f]
  (fn [& args]
    (loop [attempt 1]
      (let [result (try
                     (apply f args)
                     (catch Throwable t
                       (if (<= n attempt)
                         (throw t)
                         (do
                           (println "Retrying after following error:")
                           (.printStackTrace t)
                           ::retry))))]
        (if (= ::retry result)
          (do
            (Thread/sleep ms)
            (recur (inc attempt)))
          result)))))
