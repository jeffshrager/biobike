;; Lispscript

(defvar sent-message-value-counter 0)
(defvar request-counter 0)
(defvar stopped false)

(defvar ReadyStates (new Object))

(set (@ ReadyStates UNINITIALIZED)  0)
(set (@ ReadyStates LOADING)  1)
(set (@ ReadyStates LOADED)  2)
(set (@ ReadyStates INTERACTIVE)  3)
(set (@ ReadyStates COMPLETE) 4)

(set (@ ReadyStates name) (lambda (value) (ref (ARRAY 
					      "Uninitialized"
					      "Loading"
					      "Loaded"
					      "Interactive"
					      "Complete") value)))

(defun Http-transport (url)
  (set (@ this pending-requests) (new Array))
  (set (@ this url) url)
  (set (@ this to-send) (new Array))
  this)

(defmethod (Http-transport set-channel) (channel)
  (set (@ this channel) channel))

(defmethod (Http-transport request-messages) ()
  (let ((request (make-xml-http-request)))
    (log (+ "[" (@ request serial-number) "] making request for messages...POST " (@ this url) "?id=" (@ this channel id)))
    (.push (@ this pending-requests) request)
    (set (@ request onreadystatechange) (make-response-handler request this))
    (.open request "POST" (+ (@ this url) "?id=" (@ this channel id)) true)
    (let ((messages (.make-messages-envelope this)))
      (.send request messages))))

(defmethod (Http-transport send) (message)
  (.enqueue-outgoing-message this message)
  (.request-messages this))

(defmethod (Http-transport enqueue-outgoing-message) (message)
  (.push (@ this to-send) message))

(defmethod (Http-transport dequeue-outgoing-message) (message)
  (.shift (@ this to-send) message))

(defmethod (Http-transport make-messages-envelope) ()
  (let ((messages (make-document-object "" "MESSAGES" null)))
    (while (< 0 (@ (@ this to-send) length))
      (let ((message (.dequeue-outgoing-message this)))
	(.append-child 
	 (@ messages document-element)
	 (import-node messages (@ message document-element) true))))
    messages))

(defun make-message (value)
  (let ((message (make-document-object "" "MESSAGE" null)))
    (.append-child (@ message document-element) (.create-text-node message value))
    message))

(defmethod (Http-transport start) ()
  (log "Http-transport START, requesting msgs.")
  (.request-messages this))

(defmethod (Http-transport shutdown) ()
  (log "Http-transport SHUTDOWN called!")
  (while (> (@ this pending-requests length) 0)
    (let ((request (.pop (@ this pending-requests))))
      (.abort request))))

(defun make-response-handler (request transport)
  (lambda ()
    (let ((stop-protocol 0))
      (when (= (@ request readyState) ReadyStates.COMPLETE)
        (set (@ transport pending-requests) 
             (remove request (@ transport pending-requests)))
        (cond
         ((= (@ request status) 200)
          (log (+ "[" (@ request serial-number) "] got response: " 
                  (@ request responseText)))
          ;; probably should report the error or something. But we don't
          ;; want to choke on malformed messages such as 
          ;; timeout messages from the server.
          #+(OR)(ignore-errors 
                  (process-messages
                   (@ (@ (@ request responseXML) documentElement))))
          (process-messages (@ (@ (@ request responseXML) documentElement))))
         ((= (@ request status) 404)
          (.write 
           document 
           (+
            "<h3> Ruh roh!  The server may have been rebooted!  </h3>"
            "<b>You must log in again.</b>  "
            "<p>This session cannot continue!"
            ))
          (set stop-protocol 1))
          
         (true
          #+(OR)(debug "Got request status: " (@ request status))))
      
        (when (and (= 0 stop-protocol) 
                   (= (@ transport pending-requests length) 0))
          (.request-messages transport))))))

#+obsolete
(defun make-response-handler (request transport)
  (lambda ()
    (when (= (@ request readyState) ReadyStates.COMPLETE)
      (set (@ transport pending-requests) (remove request (@ transport pending-requests)))
      (cond
	((= (@ request status) 200)
	 (log (+ "[" (@ request serial-number) "] got response: " (@ request responseText)))
	 ;; probably should report the error or something. But we don't
	 ;; want to choke on malformed messages such as timeout messages from the server.
	 #+(OR)(ignore-errors (process-messages (@ (@ (@ request responseXML) documentElement))))
	 (process-messages (@ (@ (@ request responseXML) documentElement))))
	(true
	 #+(OR)(debug "Got request status: " (@ request status))))
      
      (when (= (@ transport pending-requests length) 0)
	(.request-messages transport)))))

(defun process-messages (messages-document)
  (let ((messages (@ messages-document childNodes)))
    (log (+ (@ messages length) " messages to be processed, in order."))
    (dotimes (i (@ messages length))
      (let ((message (.item messages i)))
	(log (+ "  Trying to receive message: " (@ message nodeName))) 
	(.receive (@ this channel) message)))))

(defun make-xml-http-request ()
  (let ((request
	 (cond
	   ;; Standards compliant browsers (Mozilla based / Safari)
	   ((@ window XMLHttpRequest) 
	    (new XMLHttpRequest))
	   ;; IE Browsers
	   ((@ window ActiveXObject)
	    (or (ignore-errors (new ActiveXObject "Msxml2.XMLHTTP"))
		(new ActiveXObject "Microsoft.XMLHTTP")))
	   ;; Ice Browser?
	   ((@ window create-request)
	    (ignore-errors (.create-request window)))
	   (true
	    (throw (new Error "Unable to create a HTTP Request object"))
	    false)
	 )))
    (set (@ request birthdate) (new Date))
    (set (@ request serial-number) (++ request-counter))
    request))

(defun position (item sequence)
  (dotimes (i (@ sequence length))
    (when (eq item (ref sequence i)) (return-from position i)))
  null)

(defun remove (item sequence)
  (.splice sequence (position item sequence) 1)
  sequence)

(defvar transport (new HttpTransport "/ajax/messaging"))

(defvar channel (new Channel transport))

(defun start-transport () (.start transport))

(defun shutdown-transport () (.shutdown transport))

(defun stop-requests () (set stopped true))

(defun another-request () (.request-messages transport))

(defun queue-to-send () (.enqueue-outgoing-message transport (make-message (+ "" (++ sent-message-value-counter)))))

(defun actually-send () 
	(.send transport (make-message (+ "" (++ sent-message-value-counter)))))

(defun channel-send () (.send channel (make-message (+ "" (++ sent-message-value-counter)))))

(defun log (text)
  (let ((debugging (.getElementById document "debugging")))
    (when debugging
      (+= (@ debugging innerHTML) (+ "<p><font color='#0000aa'> " text "</font>")))))

#+(or)(defun log-message (message)
  (let ((text (@ message textContent)))
    (+= (@ (.getElementById document "messages") innerHTML) (+ "<p><font color='#00aa00'> " text "</font>"))))

#+(or)(defun log-for-channel (text)
  (let ((element (.getElementById document "channel-log")))
    (when element
      (+= (@  element innerHTML) (+ "<p><font color='#00aaaa'> " text "</font>")))))

#+(or)(set (@ channel logger) log-for-channel)

(defmethod (Http-transport delayed-request) (delay)
  (log (+ "Will request messages again after " delay " msec."))
  (unless stopped
    (let ((transport this))
      (.set-timeout 
       window
       (lambda () (.request-messages transport))
       delay))))

