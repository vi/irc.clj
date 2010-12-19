(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])
(use 'clojure.contrib.server-socket)
(require 'clojure.contrib.string )
(use '[clojure.contrib.string :only [split join upper-case lower-case trim blank?]])

(defn log [& args] (. java.lang.System/out println (apply str (interpose "|\t" args))))
(defn ircmsg [user code text & args]
 (locking *out* (print (format ":irc.clj %s %s %s\r\r\n" code user (apply format text args)))))
(defn ^{:private true} ircmsg2-impl [from cmd msg]
 (locking *out* (print (format ":%s %s %s" from cmd msg))))
(defn ircmsg2 [from cmd & args] 
 (locking *out* (print (format ":%s %s %s :%s\r\r\n" from cmd (join " " (butlast args)) (last args))) (flush)))
(def users (ref {}))
(def channels (ref {}))

(defmacro try-output-to [writer# & code] 
 `(try 
     (binding [*out* ~writer#] 
      ~@code)
     (catch Exception e# (.printStackTrace e#))))

(defn greet [newuser]
 (ircmsg newuser "001" "Welcome to _Vi's Clojure IRC \"server\"")
 (ircmsg newuser "005" "PREFIX=(ov)@+ NETWORK=demo CHANTYPES=# : are supported by this demo") 
 (ircmsg newuser "251" ":There are %d users on the server." 0)
 (ircmsg newuser "254" "%d :channels formed" 0)
 (ircmsg newuser "375" "MoTH"))
(defn get-userid [nick] (lower-case nick))
(defn unregister-user [user]
 (let [userid (get-userid user)
  usrs (dosync (alter users #(dissoc %1 userid)) @users)]
  (doall (map #(try-output-to (get (second %1) :out) 
		(ircmsg2 user "QUIT" user "Connection closed")) usrs))
  (dosync (alter channels #(into {} (for [[k v] %1] [k (disj v userid)])))) ))
(defmulti cmd (fn [^String user cmd & args] cmd))
    (defmethod cmd "NICK" [user _ & args]
     (if (empty? args)
      (do (ircmsg user "431" ":No nickname given") user)
      (let [newuser (first args)]
       (if (re-find #"^[\]\[{}\\|_^a-zA-Z][\]\[{}\\|_^a-zA-Z0-9]{0,29}$" newuser) 
	(let [
	 userid (get-userid newuser)
	 already-present (dosync
	  (if (contains? @users userid)
	    true
	    (do (alter users #(conj %1 {userid {:nick newuser, :out *out*, :userid userid}})) false)
	    ))
	 ]
	 (if already-present
	  (do 
	   (ircmsg user "433" "%s :Nickname already in use." newuser)
	   user)
	  (do 
	   (if (= user "*")
	    (greet newuser)
	    (let [olduserid (get-userid user)
	     users (dosync (alter users #(dissoc %1 olduserid)) @users)]
	     (doall (map 
		     #(try-output-to (get (second %1) :out) 
			 (ircmsg2 user "NICK" newuser))
	      users))
	      (dosync (alter channels #(into {} (for [[k v] %1] [k (if (contains? v olduserid) (conj (disj v olduserid) userid) v)]))))))
	   newuser)))
	(do
	 (ircmsg user "432" "%s :Erroneous Nickname: Nickname should match [][{}\\|_^a-zA-Z][][{}\\|_^a-zA-Z0-9]{0,29}" newuser)
	 user)))))
    (defmethod cmd "PRIVMSG" [user cmd & args]
     (if (= (count args) 2)
       (let [recepient (first args), message (second args), ruserid (get-userid recepient)] 
	(if (= (first ruserid) \#)
	 (let [chs (dosync @channels)]
	  (if (contains? chs ruserid)
	   (doall (map #(when (not= %1 (get-userid user))
			 (try-output-to (get (get (dosync @users) %1) :out) 
			  (ircmsg2 user "PRIVMSG" recepient message))) 
		   (get chs ruserid)))
	   (ircmsg user "401" "%s :No such nick/channel" recepient)))
	 (let [usrs (dosync @users)]
	  (if (contains? usrs ruserid)
	   (try-output-to (get (get usrs ruserid) :out) 
	     (ircmsg2 user "PRIVMSG" recepient message))
	   (ircmsg user "401" "%s :No such nick/channel" recepient)))))
       (ircmsg user "412" ":There should be exactly two arguments for PRIVMSG")))
    (defmethod cmd "JOIN" [user cmd & args]
     (if (= (count args) 1)
      (let [channel (lower-case (trim (first args)))]
       (if (re-find #"^#[\#\]\[{}\\|_^a-zA-Z0-9]{0,29}$" channel)
	;PART #dsfsdf
	;:pratchett.freenode.net 403 _VI2 #dsfsdf :No such channel
	;PART #freenode
	;:pratchett.freenode.net 442 _VI2 #freenode :You're not on that channel
	(do (dosync (alter channels (fn[chs] (update-in chs [channel] #(conj (set %1) (get-userid user)))))) 
	 (let [chs (dosync @channels), ch (get chs channel)]
	  (doall (map 
		  #(try-output-to (get (dosync (get @users (get-userid %1))) :out)
		      (ircmsg2 user "JOIN" channel)) ch))
	  (ircmsg user "353" "@ %s :%s" channel (join " " ch))
	  (ircmsg user "366" "%s :End of /NAMES list." channel)
	  ))
	(ircmsg user "479" "%s :Illegal channel name" channel) ))
      (ircmsg user "412" ":There should be exactly one argument for JOIN")))
    (defmethod cmd "USER" [user cmd & args])
    (defmethod cmd "QUIT" [user cmd & args])
    (defmethod cmd :default [user cmd & args] 
     (ircmsg user "421" "%s: Unknown command" cmd))
    (defmethod cmd "TEST" [user & args]
     (doall (map #(ircmsg user "421" "TEST :Parameter is \"%s\"" %) args)))
    (defmethod cmd "PING" [user _ & args]
      (println ":irc.clj PONG irc.lcj :irc.clj"))
    (defmethod cmd "DEBUG" [user _ & args]
     (ircmsg user "000" (format ": Debug %s" (dosync [@users @channels]))))

(defn process-user-input [user ^String line] 
 (let [result (re-find #"(\w+)(.*)?" line)] 
  (if result
   (let [
    command (trim (upper-case (nth result 1)))
    params (trim (nth result 2))
    newuser (let [final-parameter-results (re-find #"(.*):(.*)" params)]
	(if (and (= user "*") (not (or (= command "NICK") (= command "DEBUG") (= command "QUIT") (= command "PING"))))
	 (do (ircmsg user "451" ":You are not registered") user)
	 (if final-parameter-results 
	  (if (blank? (nth final-parameter-results 1))
	   (apply cmd [user command (nth final-parameter-results 2)])
	   (apply cmd (concat [user command] (split #"\s+" (nth final-parameter-results 1)) [(nth final-parameter-results 2)])))
	  (if (blank? params)
	   (apply cmd [user command])
	   (apply  cmd (concat [user command] (split #"\s+" params)))))))
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
	     (if line
	      (do
	       (log user line)
	       (let [user (process-user-input user line)]
		(flush)
		(recur user)))
	      (unregister-user user))))))]
  (create-server 6667 irc)))

(def my-server (irc-server))
