;; Any functions we want to provide as an interface need to be in this package.
(in-package :com.biobike.ajax)

;;; ---------------------------------------- simple messaging ----------------------------------------

(defun reverse-a-string (string)
  "Here is the 'business logic' for our application."
  (reverse string))

(defun reverser-handler (channel message)
  "This function is the message handler for our application.
The message was sent to us from the browser.
The channel is a unique identifier to use in responding."
  (send channel (list :reverser (reverse-a-string (second message)))))

;; This tells the ajax module that any messages for the "reverser" application
;; should be handled by the function "reverser-handler".
(register-message-handler :reverser 'reverser-handler)

;;; ---------------------------------------- xml messaging ----------------------------------------

(defun reverse-three-names (msg)
  "Parse the xml message received and extract the three names.
Reverse three names and return them as xml."
  (format t "Message: ~S~%" msg)
  (destructuring-bind (app (root (a fname) (b mname) (c lname))) msg
      (format t "App: ~S~%" app)
      (format t "Root: ~S~%" root)
      (format t "a: ~S fname: ~S~%" a fname)
      (format t "b: ~S fname: ~S~%" b mname)
      (format t "c: ~S fname: ~S~%" c lname)
      (let ((emanf (reverse fname))
	    (emanm (reverse mname))
	    (emanl (reverse lname)))
	`(:reversed-names (:first ,emanf) (:middle ,emanm) (:last ,emanl)))))

(defun reverser2-handler (channel message)
  (send channel (list :reverser2 (reverse-three-names message))))

(register-message-handler :reverser2 'reverser2-handler)

;;; ---------------------------------------- server messaging ----------------------------------------

(defparameter *word-channel* nil "Saved channel to send reversed word.")
(defparameter *hard-word* nil "Word to be reversed after timer expires.")

(defun reverser3-handler (channel message)
  "Handle the client message to reverse a word.
Save the word to be reversed and the channel to return it on.
Rather than returning the answer, send a placeholder message."
  (format t "R3: ~s~%" message)
  (setf *word-channel* channel)
  (setf *hard-word* (second message))
  (send channel `(:reverser3 "*** please wait ***")))

(register-message-handler :reverser3 'reverser3-handler)

(defparameter *timer* nil "Timer to send status messages and delay reversing.")
(defparameter *status* 10 "Seconds until ready to send reply.")
(defparameter *status-channel* nil "Channel to send status messages.")

(defun send-status ()
  "Decrement status counter.  If it is greater than zero, send the current value
back to the client as a status message.  Once we reach zero, stop the time,
send a completed status message, and send the resulting reversed word."
  (decf *status*)
  (if (plusp *status*)
      (send *status-channel* `(:reverser3-status ,*status*))
      (progn
	(com.gigamonkeys.utilities:shutdown-timer *timer*)
	(send *status-channel* `(:reverser3-status "Done!"))
	(send *word-channel* `(:reverser3 ,(reverse *hard-word*))))))

(defun start-status-timer (channel)
  "Set the countdown timer value to the number of letters in the word to be reversed.
We just do this to be cute and show different countdown values and to simulate tasks
of varying length.  Save the word to be reversed, as well as the channel to send it
back.  Start the timer to send the status messages."
  (setf *status* (length *hard-word*))
  (setf *status-channel* channel)
  (setf *timer* (com.gigamonkeys.utilities:make-timer))
  (com.gigamonkeys.utilities:schedule-event *timer* 'send-status :in 1 :repeating t))

(defun reverser3-status-handler (channel message)
  "On the first request for status, return the current status countdown after starting the timer."
  (format t "R3-s: ~s~%" message)
  (start-status-timer channel)
  (send-status))

(register-message-handler :reverser3-status 'reverser3-status-handler)
