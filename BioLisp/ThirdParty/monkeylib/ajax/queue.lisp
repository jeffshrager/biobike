(in-package :com.gigamonkeys.ajax)

(defclass queue ()
  ((head :initform nil :accessor head)
   (tail :initform nil :accessor tail)
   (depth :initform 0 :accessor depth)))

(defclass threaded-queue (queue)
  ((lock :initform (mp:make-process-lock))
   (gate :initform (mp:make-gate t))))

(defmethod empty-p ((queue queue))
  (null (head queue)))

(defmethod peek ((queue queue))
  (car (head queue)))

(defmethod enqueue ((queue queue) thing)
  (with-slots (head tail depth) queue
    (incf depth)
    (setf tail
	  (if (empty-p queue)
	      (setf head (cons thing nil))
	      (setf (cdr tail) (cons thing nil))))))
  
(defmethod dequeue ((queue queue))
  (with-slots (head tail depth) queue
    (let ((item (unless (empty-p queue) (decf depth) (pop head))))
      (when (empty-p queue) (setf tail nil))
      item)))

(defmethod dequeue-all ((queue queue))
  (with-slots (head tail depth) queue
    (let ((all head))
      (setf head (setf tail nil))
      (setf depth 0)
      all)))


(defmethod empty-p ((queue threaded-queue))
  (with-slots (lock) queue
    (mp:with-process-lock (lock)
      (call-next-method))))

(defmethod peek ((queue threaded-queue))
  (with-slots (lock) queue
    (mp:with-process-lock (lock)
      (call-next-method))))

(defmethod enqueue ((queue threaded-queue) thing)
  (declare (ignore thing))
  (with-slots (lock gate) queue
    (mp:with-process-lock (lock)
      (call-next-method)
      (mp:open-gate gate))))
  
(defmethod dequeue ((queue threaded-queue))
  (with-slots (lock gate) queue
    (loop
       (mp:process-wait "Waiting on queue" #'mp:gate-open-p gate)
       (mp:with-process-lock (lock)
	 (unwind-protect 
	      (unless (empty-p queue) (return (call-next-method)))
	   (when (empty-p queue)
	     (mp:close-gate gate)))))))

(defmethod dequeue-all ((queue threaded-queue))
  (with-slots (head tail lock gate) queue
    (mp:with-process-lock (lock)
      (unwind-protect (call-next-method)
	(mp:close-gate gate)))))

#+(or)(progn
(defun empty-p (queue)
  (with-slots (head tail lock) queue
    (mp:with-process-lock (lock)
      (null head))))

(defun peek (queue)
  (with-slots (head lock) queue
    (mp:with-process-lock (lock)
      (car head))))

(defun enqueue (queue thing)
  (with-slots (head tail lock gate) queue
    (mp:with-process-lock (lock)
      (setf tail
	    (if (empty-p queue)
		(setf head (cons thing nil))
		(setf (cdr tail) (cons thing nil))))
      (mp:open-gate gate))))
  
(defun dequeue (queue)
  (with-slots (head tail lock gate) queue
    (loop
       (mp:process-wait "Waiting on queue" #'mp:gate-open-p gate)
       (mp:with-process-lock (lock)
	 (unwind-protect 
	      (unless (empty-p queue) (return (pop head)))
	   (when (empty-p queue)
	     (setf tail nil)
	     (mp:close-gate gate)))))))

(defun dequeue-all (queue)
  (with-slots (head tail lock gate) queue
    (mp:with-process-lock (lock)
      (let ((all head))
	(setf head (setf tail nil))
	(mp:close-gate gate)
	all)))))

(defun dump-queue (queue &optional (stream *standard-output*))
  (with-slots (head lock) queue
    (mp:with-process-lock (lock)
      (format stream "~s:~1{~#[ <empty>~:;~%~@{~2t~a~^~%~}~]~:}~%" queue head))))