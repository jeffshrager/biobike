(in-package :com.gigamonkeys.ajax)

(register-message-handler :repl 'repl-on-message)

(defvar *repl-contexts* (make-hash-table))
(defvar *shared-repl* nil)

(defclass repl-context ()
  ((repl-package :initform (find-package :cl-user) :accessor repl-package)
   (channels :initform nil :accessor channels)))

(defclass shared-repl-context (repl-context)
  ((channels :initform (make-hash-table) :accessor channels)
   (history :initform nil :accessor history)))

(defun intern-repl-context (channel)
  (multiple-value-bind (context found-p) (gethash channel *repl-contexts*)
    (unless found-p
      (setf (gethash channel *repl-contexts*) 
	    (setf context (make-instance 'repl-context))))
    context))

(defun attach-to-repl (channel)
  (unless *shared-repl*
    (setf *shared-repl* (make-instance 'shared-repl-context)))
  (unless (gethash channel (channels *shared-repl*))
    (setf (gethash channel (channels *shared-repl*)) t)
    (loop for message in (reverse (history *shared-repl*))
       do (send channel message)))
  *shared-repl*)

(defun repl-on-message (channel message)
  (let ((context (intern-repl-context channel)))
    (destructuring-bind (repltag (verb &rest payload)) message
      (assert (eql repltag :repl))
      (case verb
	(:eval
	 (let ((expr (format nil "~{~a~}" payload)))
	   (send channel `(:repl (:input ,expr)))
	   (with-standard-io-syntax
	     (let* ((*package* (repl-package context)))
	       (with-output-to-string (*standard-output*)
		 (multiple-value-bind (value condition) 
		     (ignore-errors 
		       (eval (read-from-string expr)))
		   (let ((output (get-output-stream-string *standard-output*)))
		     (when (plusp (length output))
		       (send channel `(:repl (:output ,output)))))
		   (unless (eql *package* (repl-package context))
		     (setf (repl-package context) *package*)
		     (send channel `(:repl (:package-change ,(short-package-name *package*)))))
		   (cond
		     (condition
		      (send channel `(:repl (:error  ,(write-to-string condition :readably nil :escape t)))))
		     (t 
		      (send channel `(:repl (:result ,(write-to-string value :readably nil :escape t))))))))))))
	(:arglist
	 #+swank
	 (let ((swank::*buffer-package* (repl-package context))
	       (swank::*buffer-readtable* *readtable*))
	   (let ((arglist (swank:arglist-for-echo-area payload)))
	     (when arglist
	       (send channel `(:repl (:arglist ,arglist)))))))))))

(defun shared-repl-on-message (channel message)
  (let ((context (attach-to-repl channel))
	(expr (format nil "~{~a~}" (rest message))))
    (flet ((send (message)
	     (push message (history context))
	     (loop for ch being the hash-keys of (channels context) do
		  (send ch message))))
      (send `(:input ,expr))
      (with-standard-io-syntax
	(let* ((*package* (repl-package context)))
	  (with-output-to-string (*standard-output*)
	    (handler-case
		(let ((sexpr (read-from-string expr)))
		  (handler-case 
		      (let ((results (multiple-value-list (eval sexpr))))
			(let ((output (get-output-stream-string *standard-output*)))
			  (when (plusp (length output))
			    (send `(:output ,output))))
			(unless (eql *package* (repl-package context))
			  (setf (repl-package context) *package*)
			  (send `(:package-change ,(short-package-name *package*))))
			(dolist (r results)
			  (send `(:result ,(write-to-string r :readably nil :escape t)))))
		    (error (e) 
		      (send `(:error ,(format nil "~a" e))))))
	      (error (e)
		(send `(:read-error ,(format nil "~a; reading ~s" e expr)))))))))))

(defparameter *canonical-nicknames*
  '(("COMMON-LISP-USER" . "CL-USER")))

(defun short-package-name (package)
  (let ((full-name (package-name package))
	(nicknames (package-nicknames package)))
    (or (cdr (assoc full-name *canonical-nicknames* :test #'string=))
	(and nicknames (reduce #'(lambda (a b) (if (< (length a) (length b)) a b)) nicknames))
	(let ((dot (position #\. full-name :from-end t)))
	  (when (and dot (< (1+ dot) (length full-name)))
	    (subseq full-name (1+ dot))))
	full-name)))
       
	
