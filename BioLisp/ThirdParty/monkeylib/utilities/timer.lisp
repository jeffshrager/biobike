;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.utilities)

(defclass timer ()
  ((events :initform (make-heap) :accessor events)
   (thread :initarg thread)
   (gate :initform (mp:make-gate nil))
   (lock :initform (mp:make-process-lock))
   (shutdown-p :initform nil :accessor shutdown-p)))

(defclass timer-event ()
  ((run-at :initarg :run-at :accessor run-at)
   (action :initarg :action :accessor action)
   (canceled-p :initform nil :accessor canceled-p)
   (repeating :initform nil :initarg :repeating :accessor repeating)))

(defmacro with-lock ((obj) &body body)
  `(mp:with-process-lock ((slot-value ,obj 'lock))
     ,@body))

(defmacro with-gate-closed-and-lock-released ((obj) &body body)
  `(with-slots (lock gate) ,obj
     (unwind-protect 
	  (progn 
	    (mp:close-gate gate)
	    (mp:process-unlock lock)
	    ,@body)
       (mp:process-lock lock))))

(defun make-timer ()
  (let ((timer (make-instance 'timer)))
    (mp:process-run-function "Timer thread." #'timer-loop timer)
    timer))

(defun shutdown-timer (timer)
  (with-lock (timer)
    (with-slots (shutdown-p gate) timer
      (setf shutdown-p t)
      (mp:open-gate gate))))

(defun restart-timer (timer)
  (unless (shutdown-p timer)
    (error "Can't restart running timer."))
  (setf (shutdown-p timer) nil)
  (mp:process-run-function "Timer thread." #'timer-loop timer))
  

(defmethod heap> ((a timer-event) (b timer-event))
  (< (run-at a) (run-at b)))

(defun schedule-event (timer action &key in at repeating)
  (unless (or in at)
    (error "Must supply either :in or :at argument."))
  (when (and repeating (not in))
    (error "Can only supply :repeating with :in"))
  (let ((utc (or at (+ in (get-universal-time)))))
    (with-lock (timer)
      (with-slots (events gate) timer
	(let ((event (make-instance 'timer-event :run-at utc :action action :repeating (and repeating in))))
	  (heap-push event events)
	(when (eql (heap-peek events) event)
	  (mp:open-gate gate))
	event)))))

(defun cancel-event (event)
  (setf (canceled-p event) t))

(defun timer-loop (timer)
  ;; Peek at heap. If next event is in future wait with timeout until
  ;; then. If no item on heap just wait.
  (with-lock (timer)
    (with-slots (thread events gate shutdown-p) timer
      (setf thread mp:*current-process*)
      (loop
	 (when shutdown-p (return))
	 (let ((event (heap-peek events)))
	   (if event
	       (if (canceled-p event)
		   (heap-pop events)
		   (let ((run-in (- (run-at event) (get-universal-time))))
		     (cond 
		       ((<= run-in 0)
			(heap-pop events)
			(handler-case (funcall (action event))
			  (error (e) (warn "Timer event signaled error: ~a" e)))
			(when (repeating event)
			  (setf (run-at event) (+ (get-universal-time) (repeating event)))
			  (heap-push event events)))
		       (t
			(with-gate-closed-and-lock-released (timer)
			  (mp:process-wait-with-timeout
			   "Waiting for event or timeout"
			   run-in 
			   #'mp:gate-open-p
			   gate))))))
	       (with-gate-closed-and-lock-released (timer)
		 (mp:process-wait "Waiting for event." #'mp:gate-open-p gate))))))))
		       