;; Lispscript

;; A reliable messaging channel. Each Channel object represents one
;; end of a bidirectional message pipe. User code can use the
;; SEND-MESSAGE method on the Channel to send messages and receives
;; messages via the ON-MESSAGE callback.

;; The Channel doesn't need to know anything about the addressing of
;; the messages it sends and receives--it is simply given opaque
;; messages that it then arranges to send to the Channel on the other
;; side of the wire, once and only once, using an unreliable
;; transport. It sends two kinds of messages, SEND messages which
;; actually contain messages given to it to be sent, and ACK messages
;; which it uses to communicate to the Channel on the other side which
;; SEND messages it has, in fact, received.

;; The Channel needs to keep a queue of outgoing messages because it
;; can't necessarily send messages right away: if it has outstanding
;; unacked messages, it needs to hold on to new outgoing messages
;; until the acks come in. So a call to SEND-MESSAGE from the client
;; code may initially result only in the Channel queuing the message.
;; Eventually, however, the Channel will pass the message along to an
;; underlying transport mechanism which will attempt to covney the
;; message to other side.

;; Channel generates send and ack messages and hands them to the
;; http-transport immediately where they are held to be sent at the
;; next available opportunity.

;; The channel wraps the messages passed to it via SEND in an XML
;; envelope that contains a sequence number and then passes the
;; envelope to the HTTP-TRANSPORT object which actually sends the
;; messages. The channel also keeps 


;; Each time we send a message we enqueue it and then send it
;; asynchronously using an XMLHttpRequest object. We keep track of
;; messages actually sent, acknowledged, and received using three
;; counters. This allows us to detect gaps and dups: each message is
;; sent with a sequence number which is acked by the recipient. When
;; the sender receives an ack, if it is the expected ack it removes
;; the message from the queue and sends the next message. Otherwise it
;; resends the message on the top of the queue, tagging it with the
;; same sequence number.


;;  Jan 02 '13  JKMyers.   
;; The following two forward function declarations are useful for cleanness;
;;  they get overridden later in vpl.js.  Used once each in 
;;  Channel.prototype.receive one-third of the way down in this file, and
;;  Channel.prototype.setAckTimeout  halfway down this file.
;(defun ServerStillAliveAndKicking_Callback () )
;(defun ServerMajorTimeout_Callback () )
;;  update:  
;;  --these forward declarations are not working properly, they are creating functions
;;      window.ServerStillAliveAndKicking_Callback()  not   ServerStillAliveAndKicking_Callback().
;;      window.ServerMajorTimeout_Callback()            not   ServerMajorTimeout_Callback().
;;  --The only reason for putting these in is if this ajax.ls/.js file gets used elsewhere,
;;      other than the VPL system, and these two javascript functions have not been defined
;;  --I am therefore yanking these useless forward declarations out
;;  -- IF YOU WANT TO USE THIS AJAX.JS FILE IN A DIFFERENT SYSTEM,
;;      YOU NOW ARE REQUIRED TO DEFINE YOUR OWN JS CALLBACK FUNCTIONS SOMEWHERE
;;           function ServerStillAliveAndKicking_Callback () 
;;           function ServerMajorTimeout_Callback () 
;;       ...or simply comment out their invocation inside this file.


(defun message-dispatcher (channel message)
  (while (= (@ message node-type) 3) ;; TEXT_NODE
    (set message (@ message next-sibling)))
  (let* ((message-type (@ message nodeName))
	 (handler (ref (@ this message-handlers) message-type)))
    (if handler
	(handler (@ message firstChild))
	(debug "No handler for message-type " message-type))))

(defun Channel (transport id)
  (set (@ this transport) transport)
  (.set-channel transport this)
  (set (@ this id) (or id (.randomID Channel)))
  (set (@ this sent) 0)
  (set (@ this acked) 0)
  (set (@ this received) 0)
  (set (@ this queue) (new Queue))
  (set (@ this send-timeout-millis) (* 10 1000))
  (set (@ this message-handlers) (new Object))
  (set (@ this on-message) message-dispatcher)
  (set (@ this logger) (lambda (text)
  (let ((debugging (.getElementById document "debugging")))
    (when debugging
      (+= (@ debugging innerHTML) (+ "<p><font color='#cc0000'> " "&lt;..." (.slice (@ this id) -3) "&gt;&nbsp;&nbsp;" text "</font>"))))
	))
  this)

(defmethod (Channel register-message-handler) (name fn)
  (set (ref (@ this message-handlers) name) fn))

(defmethod (Channel log) (text)
  ((@ this logger) text))

(define-class-method (Channel randomID) () 
  (+ (new Number (new Date)) "-" (.round Math (* (.random Math) (.pow Math 2 57)))))

;; This is the public API for sending messages. The payload should be
;; an XML DOM object representing the message to be sent. It will be
;; wrapped in a SEND message with an appropriate sequence number by
;; the Channel.

(defmethod (Channel send) (payload)
  (.log this (+ "[send] " payload))
  (let* ((sequence (.next-sequence-number this))
	 (message (make-send-xml sequence payload)))
    (.enqueue (@ this queue) message)
    (when (= (@ this sent) (@ this acked))
      (++ (@ this sent))
      (.send-send this sequence))))

;; This method is called by the transport for each incoming message.
(defmethod (Channel receive) (message-xml)
  (.log this (+ "[receive] " message-xml))
  (ServerStillAliveAndKicking_Callback)
  (let ((type (@ message-xml node-name))
	(sequence (parse-int (.get-attribute message-xml "sequence"))))
    (cond
      ((or (string= type "SEND") (string= type "send"))
       (.normalize message-xml)
       (.receive-send this sequence (@ message-xml firstChild)))
      ((or (string= type "ACK") (string= type "ack"))
       (.receive-ack this sequence))
      (true
       (alert (+ "Unknown message type: " type))))))



;; When client code asks us to send a message we need to assign it a
;; sequence number. The next sequence number is one greater than the
;; last acked sequence plus the number of unacked messages, i.e. acked
;; plus the queue depth plus one.
(defmethod (Channel next-sequence-number) ()
  (1+ (+ (@ this acked) (.depth (@ this queue)))))

(defmethod (Channel send-send) (sequence)
  (.log this (+ "[send-send] " sequence))
  (.send (@ this transport) (.peek (@ this queue)))
  (.set-ack-timeout this sequence))

(defmethod (Channel send-ack) (sequence)
  (.log this (+ "[send-ack] " sequence))
  (.send (@ this transport) (make-ack-xml sequence)))

(defmethod (Channel set-ack-timeout) (sequence)
  ;; set a timeout that will fire in N seconds and resend the message
  ;; if it hasn't been acked.
  (let ((channel this))
    (.log channel (+ "[set-ack-timeout] will fire in " (@ this send-timeout-millis) " milliseconds."))
    (.set-timeout 
     window
     (lambda () 
       (cond
	 ((>= (@ channel acked) sequence)
	  (.log channel (+ "[ack-timeout] ack received.")))
	 (true
	  (ServerMajorTimeout_Callback)
	  (.log channel (+ "[ack-timeout] ack not received, resending."))
	  (.send-send channel sequence))))
     (@ this send-timeout-millis))))

(defmethod (Channel receive-send) (sequence payload)
  (let ((expected (1+ (@ this received))))
    (cond
      ((= sequence expected)
       (.log this (+ "[receive-send@" (@ this sent) ":" (@ this acked) "] seq: " sequence ". Expected message, dispatching."))
       ((@ this on-message) this payload)
       (++ (@ this received))
       (.send-ack this (@ this received)))
      ((> sequence expected)
       (.log this (+ "[receive-send@" (@ this sent) ":" (@ this acked) "] seq: " sequence ". Gap, re-acking " (@ this received)))
       (.send-ack this (@ this received)))
      ((< sequence expected)
       (.log this (+ "[receive-send@" (@ this sent) ":" (@ this acked) "] seq: " sequence ". Dup, ignoring."))))))

(defmethod (Channel receive-ack) (ack)
  (cond
    ((and (= ack (@ this sent)) (> ack (@ this acked))) ;; message received.
     (.log this (+ "[receive-ack@" (@ this sent) ":" (@ this acked) "] ack: " ack ". Expected ack, removing message from queue."))
     (++ (@ this acked))
     (.dequeue (@ this queue))
     (when (not (.empty-p (@ this queue)))
       (++ (@ this sent))
       (.send-send this (@ this sent))))
    (true
     (.log this (+ "[receive-ack@" (@ this sent) ":" (@ this acked) "] ack: " ack ". Supurious ack, ignoring.")))))

(defmethod (Channel toString) ()
  (+ (@ this id) " [" (@ this sent) "/" (@ this acked) "/" (@ this received) "]; depth: " (.depth (@ this queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide a means to create an XML document.
;;
;; Based on ideas from http://www.webreference.com/programming/javascript/domwrapper/2.html
;;
;; Replaces: (.create-document (@ document implementation) <namespace> <root-element> <system-id>)
(defvar make-document-object
  (cond 
    ((and (@ document implementation)
          (@ document implementation create-document)
	  )
      (lambda (namespace root-element system-id)
        (.create-document (@ document implementation) namespace root-element system-id)))
    ((@ window Active-x-object)
      (let ((active-x-types (new Array))
	    (make-object null)
	    )
	(.push active-x-types "Microsoft.XMLDOM")
	(.push active-x-types "MSXML4.DOMDocument")
	(.push active-x-types "MSXML3.DOMDocument")
	(.push active-x-types "MSXML2.DOMDocument")
	(.push active-x-types "MSXML.DOMDocument")
	
	(set make-object
	  (lambda ()
	    (or (ignore-errors (new Active-x-object (ref active-x-types 0)))
	        (and (< 1 (@ length active-x-types))
		     (progn
		       (.unshift active-x-types)
		       (make-object)))
		(progn
		  (throw (new Error "Unable to create an XML Document Object"))
		  false)
		)))
		  
        (lambda (namespace root-element system-id)
          (let ((obj (make-object)))
	    (set (@ obj async) "false")
	    (when root-element
	      (if namespace
	        ;; Based on Mozilla's result
		(.loadXML obj (+ "<a0:" root-element " xmlns:a0='" namespace "'></a0:" root-element ">"))
		;else
		(.loadXML obj (+ "<" root-element "></" root-element ">")))
	    )
	    obj))))
    (true
      (throw (new Error "Unable to create an XML Document Object"))
      false)
  ))

(defvar import-node
  (cond
    ((@ document import-node)
      (lambda (document node deep-copy)
        (.import-node document node deep-copy)))
    (true
      (lambda (document node deep-copy)
        (if (@ document import-node)
	  (.import-node document node deep-copy)
	  ;else
	  (case (@ node node-type)
	    (1 ;; ELEMENT_NODE
	      (let ((cloned (.create-element document (@ node node-name))))
	        (when (@ node attributes)
		  (dotimes (i (@ node attributes length))
		    (let ((attribute (.item (@ node attributes) i)))
		      (.set-attribute cloned (@ attribute name) (@ attribute value))
		    )
		  )
		)
		(when (and deep-copy (.has-child-nodes node))
		  (let ((child-clone null)
			(child (@ node first-child))
		  	)
		    (while child
		      (set child-clone (import-node document child deep-copy))
		      (when child-clone
			(.append-child cloned child-clone)
		      )
		      (set child (@ child next-sibling))
		    )
		  )
		)
		cloned
	      ))
	    (3 ;; TEXT_NODE
	      (.create-text-node document (@ node node-value)))
	    (4 ;; CDATA_SECTION_NODE
	      (.create-text-node document (@ node node-value)))
	    (8 ;; COMMENT_NODE
	      (.create-text-node document (@ node node-value)))
	  ))))
  )
)

(defun make-send-xml (sequence payload)
  (let ((message (make-document-object "" "SEND" null)))
    (.set-attribute (@ message document-element) "SEQUENCE" (+ "" sequence))
    (.append-child 
     (@ message document-element)
     (import-node message (@ payload document-element) true))
    message))

(defun make-ack-xml (sequence)
  (let ((message (make-document-object "" "ACK" null)))
    (.set-attribute (@ message document-element) "SEQUENCE" (+ "" sequence))
    message))

(defun make-simple-message-for-app (app value)
  (let ((message (make-document-object "" app null)))
    (.append-child (@ message document-element) (.create-text-node message value))
    message))

(defun make-message-for-app (app value)
  (let ((message (make-document-object "" app null)))
    (.append-child 
     (@ message document-element)
     (import-node message (@ value document-element) true))
    message))
