;;;; Demo Clojure IRC server
;;;; Limitations:
;;;; 	No server-server connect
;;;; 	Only partially implemented basic commands (channel and private messages)
;;;; 	    no channel modes, no ops, no bans
;;;; 	Straightforward algorithms, no optimisation
;;;; Implemented by Vitaly "_Vi" Shukela, 2010; LGPL.
(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])
(use 'clojure.contrib.server-socket)
(require 'clojure.contrib.string )
(use '[clojure.contrib.string :only [split join upper-case lower-case trim blank?]])

(defn log [& args] (. java.lang.System/out println (apply str (interpose "|\t" args))))

(defn irc-reply [user code text & args] "Print text like \":irc.clj 251 q :There are 2 users on the server\""
 (locking *out* (print (format ":irc.clj %s %s %s\r\r\n" code user (apply format text args)))))
(defn ^{:private true} ircmsg2-impl [from cmd msg]
 (locking *out* (print (format ":%s %s %s\r\r\n" from cmd msg)) (flush) ))
(defn irc-event  "Print text like \":_Vi JOIN #qqq :User joined the channel\""
 ([from cmd solearg] (ircmsg2-impl from cmd (str ":" solearg)))
 ([from cmd arg & args] (ircmsg2-impl from cmd (str (join " " (cons arg (butlast args))) " :" (last args)))))

(def users (ref {}))
(def channels (ref {}))
(def channel-topics (ref {}))

(defmacro try-output-to [writer# & code] 
 `(try 
     (binding [*out* ~writer#] 
      ~@code)
     (catch Exception e# (.printStackTrace e#))))

(defn greet [newuser] "Welcome message for user (after \"NICK\" command)"
 (irc-reply newuser "001" "Welcome to _Vi's Clojure IRC \"server\"")
 (irc-reply newuser "005" "PREFIX=(ov)@+ NETWORK=demo CHANTYPES=# : are supported by this demo") 
 (irc-reply newuser "251" ":There are %d users on the server." (count (dosync @users)))
 (irc-reply newuser "254" "%d :channels formed" (count (dosync @channels)))
 (irc-reply newuser "375" "MoTH"))
(defn get-user-id [nick] (lower-case (trim nick)))

(defn broadcast [function] "Send message to all users"
 (doall (map #(try-output-to (get (second %1) :out) 
	       (function)) (dosync @users))))

(defn channel-multicast "Send message to all channel participants (except of ignore-user)" 
 ([channel function ignore-user]
  (doall (map #(when (not= %1 ignore-user)
		(try-output-to (get (get (dosync @users) %1) :out) 
		 (function))) 
	  channel)))
 ([channel function] (channel-multicast channel function nil)))

(defn remove-user! [user-id] 
 (dosync (alter users #(dissoc %1 user-id))))
(defn add-user! [user-id nick] "Add user. If already exists returns true"
 (dosync
  (if (contains? @users user-id)
   true
   (do (alter users #(conj %1 {user-id {:nick nick, :out *out*, :user-id user-id}})) false)
  )))
(defn change-nick-on-all-channels! [old-user-id new-user-id]
 (dosync (alter channels #(into {} (for [[k v] %1] [k (if (contains? v old-user-id) (conj (disj v old-user-id) new-user-id) v)])))))
(defn remove-user-from-channels [user-id]
  (dosync (alter channels #(into {} (for [[k v] %1] [k (disj v user-id)])))))
(defn join-channel! [user-id channel] "Create channel or add user to it"
 (dosync 
  (alter channel-topics (fn [chs] (update-in chs [channel] #(if %1 %1 ""))))
  (alter channels (fn[chs] (update-in chs [channel] #(conj (set %1) user-id))))))
(defn part-channel! [user-id channel] "Remove user from channel. Returns false if channel does not exist"
 (dosync 
  (if (contains? @channels channel)
   (do (alter channels (fn[chs] (update-in chs [channel] #(disj %1 user-id)))) true)
   false)))
(defn update-channel-topic! [channel new-topic] "Update topic on channel. Returns false if no such channel"
 (dosync 
  (if (contains? @channels channel)
   (do (alter channel-topics (fn[chs] (update-in chs [channel] (fn[_]new-topic)))) true)
   false)))
 
(defn unregister-user [user]
 (let [user-id (get-user-id user)]
  (remove-user! user-id)
  (broadcast #(irc-event user "QUIT" user "Connection closed"))
  (remove-user-from-channels user-id)))

(defmulti cmd (fn [^String user cmd & args] cmd))
    (defmethod cmd "NICK" 
     ([user _] (irc-reply user "431" ":No nickname given") user)
     ([user _ nick & args] (irc-reply user "431" ":Extra arguments for NICK") user) 
     ([user _ nick]
       (if (re-find #"^[\]\[{}\\|_^a-zA-Z][\]\[{}\\|_^a-zA-Z0-9]{0,29}$" nick) 
	(let [user-id (get-user-id nick)
	 already-present (add-user! user-id nick)]
	 (if already-present
	  (do 
	   (irc-reply user "433" "%s :Nickname already in use." nick)
	   user)
	  (do 
	   (if (= user "*")
	    (greet nick) ; if it is the first "NICK" command in this session then greet user
	    (let [old-user-id (get-user-id user)] ; else it is renaming
	     (remove-user! old-user-id)
	     (broadcast #(irc-event user "NICK" nick))
	     (change-nick-on-all-channels! old-user-id user-id)))
	   nick)))
	(do
	 (irc-reply user "432" "%s :Erroneous Nickname: Nickname should match [][{}\\|_^a-zA-Z][][{}\\|_^a-zA-Z0-9]{0,29}" nick)
	 user))))
    (defmethod cmd "PRIVMSG" 
     ([user cmd recepient message]
      (let [ruser-id (get-user-id recepient)] 
       (if (= (first ruser-id) \#)
	(let [chs (dosync @channels)]
	 (if (contains? chs ruser-id)
	  (channel-multicast (get chs ruser-id) #(irc-event user "PRIVMSG" recepient message) (get-user-id user))
	  (irc-reply user "401" "%s :No such nick/channel" recepient)))
	(let [usrs (dosync @users)]
	 (if (contains? usrs ruser-id)
	  (try-output-to (get (get usrs ruser-id) :out) 
	   (irc-event user "PRIVMSG" recepient message))
	  (irc-reply user "401" "%s :No such nick/channel" recepient))))))
      ([user cmd] (irc-reply user "412" ":Not enought arguments for PRIVMSG"))
      ([user cmd recepient] (irc-reply user "412" ":Not enought arguments for PRIVMSG"))
      ([user cmd recepient message & args] (irc-reply user "412" ":Extra arguments for PRIVMSG")))
    (defmethod cmd "JOIN" 
     ([user cmd channel-name]
      (let [channel (get-user-id channel-name)]
       (if (re-find #"^#[\#\]\[{}\\|_^a-z0-9]{0,29}$" channel)
	(do 
	 (join-channel! (get-user-id user) channel) 
	 (let [chs (dosync @channels), ch (get chs channel)]
	  (channel-multicast ch #(irc-event user "JOIN" channel "User joined the channel"))
	  (irc-reply user "332" "%s :%s" channel (dosync (get @channel-topics channel)))
	  ;(irc-reply user "333" "%s :none 0" channel)
	  (irc-reply user "353" "@ %s :%s" channel (join " " ch))
	  (irc-reply user "366" "%s :End of /NAMES list." channel)
	  ))
	(irc-reply user "479" "%s :Illegal channel name" channel) )))
      ([user cmd] (irc-reply user "412" ":Not enought arguments for JOIN"))
      ([user cmd channel & args] (irc-reply user "412" ":Extra arguments for JOIN")))
    (defmethod cmd "PART" 
     ([user cmd channel-name message]
       (let [channel (get-user-id channel-name), user-id (get-user-id user)]
	(if(part-channel! user-id channel) 
	 (broadcast #(irc-event user "PART" channel message))
	 (irc-reply user "403" "%s :No such channel" channel))))
      ([user cmd] (irc-reply user "412" ":Not enought arguments for PART"))
      ([user cmd channel message & args] (irc-reply user "412" ":Extra arguments for PART"))
      ([user cmd2 channel] (cmd user "PART" channel "User have left this channel")))
    (defmethod cmd "TOPIC" 
     ([user cmd channel-name new-topic]
      (let [channel (get-user-id channel-name)]
       (if (update-channel-topic! channel new-topic)
	(broadcast #(irc-event user "TOPIC" channel new-topic))
	(irc-reply user "403" (format "%s :No such channel" channel)))))
      ([user cmd] (irc-reply user "412" ":Not enought arguments for TOPIC"))
      ([user cmd channel] (irc-reply user "412" ":Not enought arguments for TOPIC"))
      ([user cmd channel topic & args] (irc-reply user "412" ":Extra arguments for TOPIC")))
    (defmethod cmd "USER" [user cmd & args])
    (defmethod cmd "QUIT" [user cmd & args])
    (defmethod cmd :default [user cmd & args] 
     (irc-reply user "421" "%s: Unknown command" cmd))
    (defmethod cmd "TEST" [user & args]
     (doall (map #(irc-reply user "421" "TEST :Parameter is \"%s\"" %) args)))
    (defmethod cmd "PING" [user _ & args]
      (println ":irc.clj PONG irc.lcj :irc.clj"))
    (defmethod cmd "LIST" [user _ & args]
     (doall (for [[k v] (dosync @channels)] (irc-reply user "322" (format "%s %d :%s" k (count v) (dosync (get @channel-topics k)))) ))
     (irc-reply user "323" ":End of /LIST"))
    (defmethod cmd "DEBUG" [user _ & args]
     (irc-reply user "000" (format ": Debug %s" (dosync [@users @channels @channel-topics]))))

(defn process-user-input [user ^String line] 
 (let [result (re-find #"(\w+)(.*)?" line)] 
  (if result
   (let [
    command (trim (upper-case (nth result 1)))
    params (trim (nth result 2))
    newuser (let [final-parameter-results (re-find #"(.*):(.*)" params)]
	(if (and (= user "*") (not (or (= command "NICK") (= command "DEBUG") (= command "QUIT") (= command "PING"))))
	 (do (irc-reply user "451" ":You are not registered") user)
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
	   (irc-reply "*" "439" ":Supply your NICK to procceed")
	   (flush)
	   (loop [user "*"]
	    (let [line (read-line)]
	     (if line
	      (do
	       (log user line)
	       (recur (try
		       (let [user (process-user-input user line)]
			(flush)
			user)
		       (catch Exception e (.printStackTrace e) (irc-reply user "400" ":Error") (flush) user))))
	      (unregister-user user))))))]
  (create-server 6667 irc)))

(def my-server (irc-server))
