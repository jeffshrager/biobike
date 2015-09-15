(in-package :com.gigamonkeys.ajax)

(defvar *message-handlers* (make-hash-table))
(defvar *message-handlers-lock* (mp:make-process-lock))

(defun register-message-handler (name handler)
  (mp:with-process-lock (*message-handlers-lock*)
    (setf (gethash name *message-handlers*) handler)))

(defun lookup-message-handler (name)
  (mp:with-process-lock (*message-handlers-lock*)
    (gethash name *message-handlers*)))

(defun message-dispatcher (channel message)
  (funcall 
   (or (lookup-message-handler (car message)) #'default-message-handler)
   channel message))

(defun default-message-handler (channel message)
  (warn "No handler for message ~a from channel ~a" message (id channel)))