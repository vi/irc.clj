(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])
(use 'clojure.contrib.server-socket)
(use 'clojure.contrib.string)
(defn log [& args] (. java.lang.System/out println (apply str (interpose "|" args))))
(defn ircmsg [user code text & args]
 (print (format ":irc.clj %s %s %s\r\r\n" code user (apply format text args))))

(defmulti cmd (fn [^String user cmd & args] cmd))
(defmethod cmd "NICK" [user _ newuser & args]
  (if (re-find #"^[\]\[{}\\|_^a-zA-Z][\]\[{}\\|_^a-zA-Z0-9]{0,29}$" newuser) 
   (do
      (ircmsg newuser "001" "Welcome to _Vi's Clojure IRC \"server\"")
      (ircmsg newuser "005" "TOPICLEN=65536 PREFIX=(ov)@+ NETWORK=demo CHANTYPES=# : are supported by this demo") 
      (ircmsg newuser "251" ":There are %d users on the server." 0)
      (ircmsg newuser "254" "%d :channels formed" 0)
      (ircmsg newuser "375" "MoTH")                                                                               
      newuser
   )
   (do
     (ircmsg user "432" "%s :Erroneous Nickname: Illegal characters" newuser)
     user
   )
  )
)
(defmethod cmd :default [user cmd & args] 
    (ircmsg user "421" "%s: Unknown command" cmd)
)
(defmethod cmd "TEST" [user & args]
    (doall (map #(ircmsg user "421" "TEST :Parameter is \"%s\"" %) args))
)
(defmethod cmd "PING" [user _ whom & args]
    (if (= whom "irc.clj")
    (println ":irc.clj PONG irc.lcj :irc.clj")
    nil ; not implemented
    )
)

(defn process-user-input [user ^String line] 
 (let [result (re-find #"(\w+)(.*)?" line)] 
  (if result
    (let [
     command (trim (upper-case (nth result 1)))
     params (trim (nth result 2))
     newuser (let [final-parameter-results (re-find #"(.*):(.*)" params)]
	 (if final-parameter-results 
	  (if (blank? (nth final-parameter-results 1))
	    (apply cmd [user command (nth final-parameter-results 2)])
	    (apply cmd (concat [user command] (split #"\s+" (nth final-parameter-results 1)) [(nth final-parameter-results 2)])))
	  (if (blank? params)
	    (apply cmd [user command])
	    (apply  cmd (concat [user command] (split #"\s+" params))))))
     ]
     (if (= command "NICK") newuser user))
    user)))

(defn irc-server []
  (letfn [(irc [in out]
                    (binding [*in* (BufferedReader. (InputStreamReader. in))
                              *out* (OutputStreamWriter. out)]
		      (ircmsg "*" "439" ":Supply your NICK to procceed")
		      (flush)
                      (loop [user "*"]
                        (let [line (read-line)]
			 (when line
			     (log line)
			     (let [user (process-user-input user line)]
				 (flush)
				 (recur user))
			 ) ; when input
			) ; read-line
                      )))]
    (create-server 6667 irc)))

(def my-server (irc-server))
