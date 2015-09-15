(in-package :com.gigamonkeys.ajax)

(defparameter *default-send-timeout-seconds* 10)

(defclass channel ()
  ((transport            :initarg :transport :accessor transport)
   (id                   :initarg :id :accessor id)
   (timer                :initarg :timer :initform (make-timer))
   (sent                 :initform 0 :accessor sent)
   (acked                :initform 0 :accessor acked)
   (received             :initform 0 :accessor received)
   (outbound-messages   
    :initform (make-instance 'queue)
    :accessor outbound-messages)
   (send-timeout-seconds
    :initarg :send-timeout-seconds
    :initform *default-send-timeout-seconds*
    :accessor send-timeout-seconds)
   (on-message
    :initarg :on-message
    :initform 'default-on-message-handler
    :accessor on-message)
   (last-touched :initform 0 :accessor last-touched)
   (dead-p :initform nil :accessor dead-p)
   (lock :initform (mp:make-process-lock))))

(defmethod print-object ((obj channel) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID: ~A alive? ~A" (id obj) (not (dead-p obj)))))

(defun pending-request-p (channel)
  (not (empty-p (gates (transport channel)))))

(defun touch (channel)
  (setf (last-touched channel) (get-universal-time)))

(defun age (channel)
  (- (get-universal-time) (last-touched channel)))

(defun default-on-message-handler (channel message) 
  (declare (ignore message))
  (warn "No on-message handler for channel ~s" channel))

(defclass queue-transport ()
  ((messages :initform (make-instance 'threaded-queue) :accessor messages)
   (gates    :initform (make-instance 'threaded-queue) :accessor gates)))

(defmethod print-object ((obj queue-transport) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~d messages; ~d waiters" (depth (messages obj)) (depth (gates obj)))))

(defmethod send ((transport queue-transport) xml)
  ;;(format t "Sending on transport: ~a~%" xml)
  ;;(when (zerop (random 20)) (return-from send))
  (with-slots (messages gates) transport
    (enqueue (messages transport) xml)
    #+(or)(if (empty-p gates)
	      (warn "Nobody waiting on transport ~a" transport)
	      (mp:open-gate (dequeue gates)))
    (when (not (empty-p gates))
      (mp:open-gate (dequeue gates)))))

;;; Channel public API

(defmethod send ((channel channel) payload)
  "Public API for sending messages. Message may or may not be
sent immediately, depending on whether we're waiting for an
ACKfor a previously sent message."
  (with-slots (outbound-messages sent acked lock) channel
    (mp:with-process-lock (lock)
      (let* ((sequence (next-sequence-number channel))
	     (message (make-send-xml sequence payload)))
	(enqueue outbound-messages message)
	(when (= sent acked)
	  (incf sent)
	  (send-send channel sequence))))))

(defmethod receive ((channel channel) message-xml)
  "Called by the server for each incoming message."
  (with-slots (lock) channel
    (mp:with-process-lock (lock)
      (let ((type (node-name message-xml))
	    (sequence (parse-integer (get-attribute message-xml :sequence))))
	(case type
	  (:send (receive-send channel sequence (first-child message-xml)))
	  (:ack (receive-ack channel sequence))
	  (t (warn "Unknown message type: ~a" type)))))))

(defmethod next-sequence-number ((channel channel))
  (with-slots (acked outbound-messages) channel
    (1+ (+ acked (depth outbound-messages)))))

(defmethod send-send ((channel channel) sequence)
  (with-slots (transport outbound-messages) channel
    (send transport (peek outbound-messages))
    (unless (dead-p channel)
      (set-ack-timeout channel sequence))))

(defmethod send-ack ((channel channel) sequence)
  (with-slots (transport) channel
    (send transport (make-ack-xml sequence))))

(defun test-channel (channel)
  (with-slots (received) channel
    (send-ack channel received)))

(defmethod set-ack-timeout ((channel channel) sequence)
  (with-slots (timer acked) channel
    (schedule-event 
     timer 
     #'(lambda ()
	 (when (< acked sequence)
	   (send-send channel sequence)))
     :in (send-timeout-seconds channel))))

(defmethod receive-send ((channel channel) sequence payload)
  (with-slots (received on-message) channel
    (let ((expected (1+ received)))
      ;;(format t "[receive-send] received: ~d; sequence: ~d" received  sequence)
      (cond
	((= sequence expected)
	 (funcall on-message channel payload)
	 (incf received)
	 (send-ack channel received))
	((> sequence expected)
	 (send-ack channel received))
	((< sequence expected))))))

(defmethod receive-ack ((channel channel) sequence)
  (with-slots (sent acked outbound-messages) channel
    (when (and (= sequence sent) (> sequence acked))
      (incf acked)
      (dequeue outbound-messages)
      (when (not (empty-p outbound-messages))
	(incf sent)
	(send-send channel sent)))))

(defun make-send-xml (sequence payload)
  `(:send :sequence ,sequence ,payload))

(defun make-ack-xml (sequence)
  `(:ack :sequence ,sequence))

;;; The few bits of the DOM we need.

(defun first-child (lxml) (cadr lxml))

(defun node-name (lxml)
  (etypecase (car lxml)
    (cons (caar lxml))
    (keyword (car lxml))))

(defun get-attribute (lxml name)
  (getf (rest (car lxml)) name))
  