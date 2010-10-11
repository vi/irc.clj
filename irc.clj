(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])
(use 'clojure.contrib.server-socket)
(defn irc-server []
  (letfn [(irc [in out]
                    (binding [*in* (BufferedReader. (InputStreamReader. in))
                              *out* (OutputStreamWriter. out)]
		      (print "001 Welcome to Clojure demo IRC server")
                      (loop []
                        (let [input (read-line)]
                          (print input)
                          (flush))
                        (recur))))]
    (create-server 6667 irc)))

(def my-server (irc-server))
