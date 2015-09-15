;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.utilities)

;; To build a timer queue on a heap. Keep a heap of scheduled items,
;; ordered by the time when they are to run, sooner items first. Each
;; time an item is added to the heap, peek at the new first item and
;; reschedule the thread to wake up at that time. When the time comes,
;; the thread wakes up, pops the item of the heap, performs it and
;; then peeks at the new first item, putting itself to sleep until
;; that item is due.

(defun make-heap (&optional (initial-size 16))
  (make-array initial-size :adjustable t :fill-pointer 0))

(defun heap-push (value heap)
  (let ((idx (length heap)))
    (vector-push-extend value heap)
    (move-up heap idx)))

(defun heap-peek (heap)
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-pop (heap)
  (unless (zerop (length heap))
    (prog1
	(aref heap 0)
      (let ((leaf-value (vector-pop heap)))
	(setf (aref heap 0) leaf-value)
	(move-down heap 0)))))

(defun empty-heap (heap)
  (fill heap nil)
  (setf (fill-pointer heap) 0))

(defgeneric heap> (a b))

(defmethod heap> ((a number) (b number)) (> a b))
(defmethod heap> ((a character) (b character)) (char> a b))
(defmethod heap> ((a string) (b string)) (string> a b))

(defun left-child-index (index)
  (1- (* 2 (1+ index))))

(defun right-child-index (index)
  (* 2 (1+ index)))

(defun parent-index (index)
  (values (floor (1- index) 2)))

(defun in-bounds-p (array idx)
  (and (array-in-bounds-p array idx)
       (< idx (fill-pointer array))))

(defun move-up (heap idx)
  (let ((parent (parent-index idx)))
    (when (and (not (minusp parent)) (heap> (aref heap idx) (aref heap parent)))
      (rotatef (aref heap idx) (aref heap parent))
      (move-up heap parent))))

(defun move-down (heap idx)
  (let ((left (left-child-index idx))
	(right (right-child-index idx)))
    (let ((larger
	   (if (in-bounds-p heap right)
	     (if (heap> (aref heap left) (aref heap right)) left right)
	     (if (in-bounds-p heap left) left))))
      (when (and larger (heap> (aref heap larger) (aref heap idx)))
	(assert (= (parent-index larger) idx))
	(rotatef (aref heap idx) (aref heap larger))
	(move-down heap larger)))))




(defun test ()
  (let ((heap (make-array 10 :adjustable t :fill-pointer 0)))
    (loop repeat 20 do (heap-push (random 100) heap))
    (let ((sorted (sort (copy-seq heap) #'>)))
      (format t "~s~%" (loop for item = (heap-pop heap)
       while item 
       do (format t "~s ~%" heap)
       collect item))
      (format t "~s~%" sorted)
      )))


#+(or)(defun node-at (heap idx)
  (aref (nodes heap) idx))

#+(or)(defun (setf node-at) (value heap idx)
  (setf (aref (nodes heap) idx) value))

#+(or)(defun items-count (heap)
  (fill-pointer (nodes heap)))

#+(or)(defun (setf items-count) (value heap)
  (setf (fill-pointer (nodes heap)) value))

#+(or)(defclass heap ()
  ((nodes :initform (make-array 10 :adjustable t :fill-pointer 0) :reader nodes)))

#+(or)(defun heap-push (value heap)
  (let ((idx (items-count heap)))
    (vector-push-extend value (nodes heap))
    (move-up heap idx)))

#+(or)(defun move-up (heap idx)
  (let ((parent (parent-index idx)))
    (when (and (not (minusp parent)) (heap> (node-at heap idx) (node-at heap parent)))
      (rotatef (node-at heap idx) (node-at heap parent))
      (move-up heap parent))))

#+(or)(defun move-down (heap idx)
  (let ((left (left-child-index idx))
	(right (right-child-index idx)))
    (let ((larger
	   (if (array-in-bounds-p (nodes heap) right)
	     (if (heap> (aref (nodes heap) left) (aref (nodes heap) right)) left right)
	     (if (array-in-bounds-p (nodes heap) left) left))))
      (when (and larger (heap> (node-at heap larger) (node-at heap idx)))
	(assert (= (parent-index larger) idx))
	(rotatef (aref (nodes heap) idx) (aref (nodes heap) larger))
	(move-down heap larger)))))

#+(or)(defun heap-pop (heap)
  (unless (zerop (length (nodes heap)))
    (prog1
	(node-at heap 0)
      (let ((leaf-value (vector-pop (nodes heap))))
	(format t "Starting with leaf ~a~%" leaf-value)
	(setf (node-at heap 0) leaf-value)
	(move-down heap 0)))))
