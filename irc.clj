(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])
(use 'clojure.contrib.server-socket)
(use 'clojure.contrib.string)
(defn log [& args] (. java.lang.System/out println (apply str (interpose "|" args))))

(defmulti cmd (fn [cmd & args] cmd))
(defmethod cmd "NICK" [_ & args]
  (println ":irc.clj 001 * Welcome to _Vi's Clojure IRC \"server\"")
  (println ":irc.clj 005 * TOPICLEN=65536 PREFIX=(ov)@+ NETWORK=demo CHANTYPES=# : are supported by this demo")
  (println ":irc.clj 251 * :There are %d users on the server.")
  (println ":irc.clj 254 * %d :channels formed")
  (println ":irc.clj 375 * MoTH")
)
(defmethod cmd :default [cmd & args] 
    (println (format ":irc.clj 421 * %s :Unknown command" cmd))
)
(defmethod cmd "TEST" [& args]
    (doall (map #(println (format ":irc.clj 421 * TEST :Parameter is \"%s\"" %)) args))
)
(defmethod cmd "PING" [_ whom & args]
    (if (= whom "irc.clj")
    (println ":irc.clj PONG irc.lcj :irc.clj")
    nil ; not implemented
    )
)

(defn process-user-input [^String line] 
 (when-let [result (re-find #"(\w+)(.*)?" line)] 
  (let [command (upper-case (nth result 1))
	params (trim (nth result 2))]
    (let [final-parameter-results (re-find #"(.*):(.*)" params)]
     (if final-parameter-results 
      (if (blank? (nth final-parameter-results 1))
	(cmd command (nth final-parameter-results 2))
	(apply cmd (concat [command] (split #"\s+" (nth final-parameter-results 1)) [(nth final-parameter-results 2)])))
      (if (blank? params)
	(cmd command)
	(apply cmd (concat [command] (split #"\s+" params))))
     )
    )
  ))
)

(defn irc-server []
  (letfn [(irc [in out]
                    (binding [*in* (BufferedReader. (InputStreamReader. in))
                              *out* (OutputStreamWriter. out)]
		      (println ":irc.clj 439 * :Supply your NICK to procceed")
		      (flush)
                      (loop []
                        (let [line (read-line)]
			 (when line
			     (log line)
			     (process-user-input line)
			     (flush)
			     (recur)
			 ) ; when input
			) ; read-line
                      )))]
    (create-server 6667 irc)))

(def my-server (irc-server))
