(in-package :com.gigamonkeys.twas)

(defparameter *default-mime-type* "application/octet-stream")

(defun mime-type-for-path (path)
  (let ((dot (position #\. path :from-end t)))
    (if dot
	(gethash (subseq path (1+ dot)) *mime-types*)
	*default-mime-type*)))

(defun file-newer-p (a b)
  (flet ((safe-write-date (file)
	   (or (file-write-date file) 0)))
    (> (safe-write-date a) (safe-write-date b))))

(defclass generated-file-entity (file-entity)
  ((source-file  :initarg :source-file :accessor source-file)
   (generator :initarg :generator :accessor generator)))


#+allegro
(defmethod process-entity :before ((req http-request) (entity generated-file-entity))
  (with-slots (net.aserve::file source-file generator) entity
    (when (file-newer-p source-file net.aserve::file)
      (funcall generator source-file net.aserve::file))))


(defun publish-generated-file (&rest args &key path host port  file content-type class preload cache-p remove
			       authorizer server timeout plist hook headers source-file generator)
  (declare (ignore path host port  file content-type preload cache-p remove
		   authorizer server timeout plist hook headers ))
  (remf args :source-file)
  (remf args :generator)
  (remf args :class)
  (let ((entity (apply #'publish-file :class (or class 'generated-file-entity) args)))
    (setf (source-file entity) source-file)
    (setf (generator entity) generator)
    entity))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spawn extra threads.

#+allegro
(defun busy-and-idle-workers ()
  (loop for p in (net.aserve::wserver-worker-threads *wserver*) 
     when (mp:process-active-p p) count t into busy
     else count t into idle
     finally (return (values busy idle))))

(defun maintain-thread-headroom (headroom)
  "Spawn extra threads if needed. Can be called from a computed
entity that is going to tie up a worker thread for a long time."
  (let ((idle (net.aserve::wserver-free-workers *wserver*)))
    (when (< idle headroom)
      (loop repeat (- headroom idle) 
	 do (net.aserve::make-worker-thread)))))
