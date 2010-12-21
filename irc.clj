;;;; Demo Clojure IRC server
;;;; Limitations:
;;;; 	No server-server connect
;;;; 	Only partially implemented basic commands (channel and private messages)
;;;; 	    no channel modes, no ops, no bans
;;;; 	Straightforward algorithms, no optimisation
;;;; 	User-id is lowercased nickname. It changes and it is exposed to user instead of nickname.
;;;;
;;;; Implemented by Vitaly "_Vi" Shukela; 2010; MIT License.

(ns irc-demo
 (:import (java.io BufferedReader InputStreamReader OutputStreamWriter))
 (:use clojure.contrib.server-socket)
 (:use [clojure.contrib.string :only [split join upper-case lower-case trim blank?]]))

;; state
(def users (ref {}))
(def channels (ref {}))
(def channel-topics (ref {}))

;; print info to stdout (*out* is overridden)
(defn log [& args] (. java.lang.System/out println (apply str (interpose "|\t" args))))

;; server to client communication
(defn irc-reply [user code text & args] "Prints text like \":irc.clj 251 q :There are 2 users on the server\""
 (locking *out* (print (format ":irc.clj %s %s %s\r\n" code user (apply format text args)))))

(defn ^{:private true} ircmsg2-impl [from cmd msg]
 (locking *out* (print (format ":%s %s %s\r\n" from cmd msg)) (flush) ))

(defn irc-event  "Print text like \":_Vi JOIN #qqq :User joined the channel\""
 ([from cmd solearg] (ircmsg2-impl from cmd (str ":" solearg)))
 ([from cmd arg & args] (ircmsg2-impl from cmd (str (join " " (cons arg (butlast args))) " :" (last args)))))

(defn greet [newuser] "Welcome message for user (after \"NICK\" command)"
 (irc-reply newuser "001" "Welcome to _Vi's Clojure IRC \"server\"")
 (irc-reply newuser "002" "I don't know what your host is")
 (irc-reply newuser "003" "I don't know when this server was created")
 (irc-reply newuser "004" "irc.clj 0.1  ")
 (irc-reply newuser "005" "PREFIX=(ov)@+ NETWORK=demo CHANTYPES=# : are supported by this demo") 
 (irc-reply newuser "251" ":There are %d users on the server." (count (dosync @users)))
 (irc-reply newuser "254" "%d :channels formed" (count (dosync @channels)))
 (irc-reply newuser "375" "MoTH")
 (irc-reply newuser "376" "End of MoTH"))

(defmacro try-output-to [writer# & code] 
 `(try 
     (binding [*out* ~writer#] 
      ~@code)
     (catch Exception e# (.printStackTrace e#))))

(defn broadcast [function] "Send message to all users. Argument is closure to execute."
 (doall (map #(try-output-to (get (second %1) :out) 
	       (function)) (dosync @users))))

(defn channel-multicast "Send message to all channel participants (except of ignore-user). Argument is closure to execute" 
 ([channel function ignore-user]
  (doall (map #(when (not= %1 ignore-user)
		(try-output-to (get (get (dosync @users) %1) :out) 
		 (function))) 
	  channel)))
 ([channel function] (channel-multicast channel function nil)))



;; state update functions

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



(defn get-user-id [nick] (lower-case (trim nick)))



;; IRC command handlers

(defmulti command (fn [^String user command-name & args] command-name))
    (defmethod command "NICK" 
     ([user _] (irc-reply user "431" ":No nickname given") user)
     ([user _ nick & args] (irc-reply user "431" ":Extra arguments for NICK") user) 
     ([user _ nick]
       (if (re-find #"^[\]\[{}\\|_^a-zA-Z][\]\[{}\\|_^a-zA-Z0-9\-]{0,255}$" nick) 
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
	 (irc-reply user "432" "%s :Erroneous Nickname: Nickname should match [][{}\\|_^a-zA-Z][][{}\\|_^a-zA-Z0-9-]{0,255}" nick)
	 user))))
    (defmethod command "PRIVMSG" 
     ([user _ recepient message]
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
      ([user _] (irc-reply user "412" ":Not enought arguments for PRIVMSG"))
      ([user _ recepient] (irc-reply user "412" ":Not enought arguments for PRIVMSG"))
      ([user _ recepient message & args] (irc-reply user "412" ":Extra arguments for PRIVMSG")))
    (defmethod command "JOIN" 
     ([user _ channel-name]
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
      ([user _] (irc-reply user "412" ":Not enought arguments for JOIN"))
      ([user _ channel & args] (irc-reply user "412" ":Extra arguments for JOIN")))
    (defmethod command "PART" 
     ([user _ channel-name message]
       (let [channel (get-user-id channel-name), user-id (get-user-id user)]
	(if(part-channel! user-id channel) 
	 (broadcast #(irc-event user "PART" channel message))
	 (irc-reply user "403" "%s :No such channel" channel))))
      ([user _] (irc-reply user "412" ":Not enought arguments for PART"))
      ([user _ channel message & args] (irc-reply user "412" ":Extra arguments for PART"))
      ([user _ channel] (command user "PART" channel "User have left this channel")))
    (defmethod command "TOPIC" 
     ([user _ channel-name new-topic]
      (let [channel (get-user-id channel-name)]
       (if (update-channel-topic! channel new-topic)
	(broadcast #(irc-event user "TOPIC" channel new-topic))
	(irc-reply user "403" (format "%s :No such channel" channel)))))
      ([user _] (irc-reply user "412" ":Not enought arguments for TOPIC"))
      ([user _ channel] (irc-reply user "332" "%s :%s" channel (dosync (get @channel-topics channel))))
      ([user _ channel topic & args] (irc-reply user "412" ":Extra arguments for TOPIC")))
    (defmethod command :default [user cmd-name & args] 
     (irc-reply user "421" "%s: Unknown command" cmd-name))
    (defmethod command "TEST" [user & args]
     (doall (map #(irc-reply user "421" "TEST :Parameter is \"%s\"" %) args)))
    (defmethod command "PING" [user _ & args]
      (println ":irc.clj PONG irc.lcj :irc.clj"))
    (defmethod command "LIST" [user _ & args]
     (doall (for [[k v] (dosync @channels)] (irc-reply user "322" (format "%s %d :%s" k (count v) (dosync (get @channel-topics k)))) ))
     (irc-reply user "323" ":End of /LIST"))
    (defmethod command "DEBUG" [user _ & args]
     (irc-reply user "000" (format ": Debug %s" (dosync [@users @channels @channel-topics]))))
    (defmethod command "USER" [user _ & args])
    (defmethod command "QUIT" [user _ & args])
    (defmethod command "MODE" [user _ & args])
    (defmethod command "" [user _ & args])

(defn unregister-user [user]
 (let [user-id (get-user-id user)]
  (remove-user! user-id)
  (broadcast #(irc-event user "QUIT" user "Connection closed"))
  (remove-user-from-channels user-id)))



(defn parse-irc-command-line [^String line] "Returns vector: command name and it's arguments, all strings. Sole empty string on empty input"
 (let [
  colon-search-result (re-find #"(.*):(.*)" line)
  ; the list of splittable parameters (including the command name) we need to split by ' ' and the final parameter after ':' (it may be nil)
  [splittable-parameters final-parameter-as-vector] 
   (if colon-search-result
    #_"example: TOPIC #qqq :Qqq qq!" [(nth colon-search-result 1) [(nth colon-search-result 2)]]
    #_"example: TOPIC #qqq Qqq"      [line                        nil])                        ]
  (into (into [] (split #"\s+" (trim splittable-parameters))) final-parameter-as-vector)))

(defn execute-irc-command-line [user ^String line] "Returns nick (possibly updated by \"NICK\" command)"
 (let [[command-name-pre & args] (parse-irc-command-line line)
  command-name (upper-case command-name-pre)]
  (if (and (= user "*") (not (contains? #{"NICK" "DEBUG" "QUIT" "PING" ""} command-name))) 
   (do (irc-reply user "451" ":You are not registered") user)
   (let [new-user (apply command user command-name args)]
    (case command-name 
     "NICK" new-user
     "QUIT" (str "~" user)
     user)))))

(defn irc-server []
 (letfn [(irc [in out]
	  (binding [*in* (BufferedReader. (InputStreamReader. in))
	   *out* (OutputStreamWriter. out)]
	   (irc-reply "*" "439" ":Supply your NICK to procceed")
	   (flush)
	   (loop [user "*"]
	    (if (not= (first user) \~); User becomes "~User" on QUIT command
	     (let [line (read-line)]
	      (if line 
	       (do
		(log user line)
		(recur (try
			(let [user (execute-irc-command-line user line)]
			 (flush)
			 user)
			(catch Exception e (.printStackTrace e) (irc-reply user "400" ":Error") (flush) user))))
	       (recur (str "~" user))))
	     (unregister-user (#_"removes the first character" apply str (next user)))))))]
  (create-server 6667 irc)))

(def my-server (irc-server))
