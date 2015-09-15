;;; -*-lisp-*-

;;; ---------------------------------------- reverse a word ----------------------------------------

(defun reverse-word ()
  (let ((word (@ (.get-element-by-id document "word") value)))
    (.send channel (make-reverser-message word))))

(defun make-reverser-message (msg)
  (make-simple-message-for-app "REVERSER" msg))

(defun update-result (text)
  (let ((result (.get-element-by-id document "reversed")))
    (set (@ result value) text)))

(defun reverser-on-message (msg)
  (update-result (@ msg textContent)))

;;; Tell client all messages for the "reverser" app are handled by "reverser-on-message".
(.register-message-handler channel "reverser" reverser-on-message)

;;; ---------------------------------------- reverse 3 names ----------------------------------------

(defun reverse-name ()
  (let ((fname (@ (.get-element-by-id document "fname") value))
	(mname (@ (.get-element-by-id document "mname") value))
	(lname (@ (.get-element-by-id document "lname") value)))
    (.send channel (make-reverse-name-message 
                    (xml (:names (:first fname) (:middle mname) (:last lname)))))))

(defun make-reverse-name-message (msg)
  (make-message-for-app "REVERSER2" msg))

(defun update-result2 (fname mname lname)
  (let ((emanf (.get-element-by-id document "emanf"))
	(emanm (.get-element-by-id document "emanm"))
	(emanl (.get-element-by-id document "emanl")))
    (set (@ emanf value) fname)
    (set (@ emanm value) mname)
    (set (@ emanl value) lname)))

(defun reverser2-on-message (msg)
  (let ((message-type (@ msg node-name)))
    (when (string= message-type "reversed-names")
      (let ((node (@ msg first-child))
	    (fname null)
	    (mname null)
	    (lname null))
	(when (string= (@ node node-name) "first")
	  (set fname (@ node text-content)))
	(set node (@ node next-sibling))
	(when (string= (@ node node-name) "middle")
	  (set mname (@ node text-content)))
	(set node (@ node next-sibling))
	(when (string= (@ node node-name) "last")
	  (set lname (@ node text-content)))
	(update-result2 fname mname lname)))))

(.register-message-handler channel "reverser2" reverser2-on-message)

;;; ---------------------------------------- reverse with timer ----------------------------------------

(defun reverse-hard-word ()
  (let ((word (@ (.get-element-by-id document "hardword") value)))
    (.send channel (make-reverser3-message word))
    (.send channel (make-reverser3-status-message 0))))

(defun make-reverser3-message (msg)
  (make-simple-message-for-app "REVERSER3" msg))

(defun make-reverser3-status-message (msg)
  (make-simple-message-for-app "REVERSER3-STATUS" msg))

(defun update-result3 (text)
  (let ((result (.get-element-by-id document "reversedHardWord")))
    (set (@ result value) text)))

(defun update-status (text)
  (let ((result (.get-element-by-id document "statusbar")))
    (set (@ result innerHTML) text)))

(defun reverser3-on-message (msg)
  (update-result3 (@ msg textContent)))

(.register-message-handler channel "reverser3" reverser3-on-message)

(defun reverser3-on-status-message (msg)
  (update-status (@ msg textContent)))

(.register-message-handler channel "reverser3-status" reverser3-on-status-message)
