;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: data-editor; -*-

(in-package :data-editor)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+


;; Author: JP Massar

#+allegro
(defvar *object-hash-lock* (mp:make-process-lock :name "Object hash lock")
  "Lock used to proctect access to *de-table*, *pid-table*, and *de-unique-counter*")

(defvar *de-table* (make-hash-table :test 'eql))

(defvar *pid-table* (make-hash-table :test 'eql))

(defvar *de-unique-counter* 0)

(defun object-from-unique-id (unique-id) 
  (with-lock (*object-hash-lock*)
    (let ((object (first (gethash unique-id *de-table*))))
      (when object (update-timestamp *de-table* unique-id object))
      object)))

(defun unique-id-from-object (object) 
  (with-lock (*object-hash-lock*)
    (gethash object *de-table*)))

(defun purge-de-table (seconds-ago)
  (with-lock (*object-hash-lock*)
    (let* ((now (get-universal-time))
	   (purge-cutoff (- now seconds-ago))
	   (ht *de-table*)
	   ;; Find all the expired timestamps in the wob hash table
	   ;; and get their associated objects and their keys which are 
	   ;; the objects unique-ids.
	   (expired-ids-and-objects
	    (lmaphashnn
	     (lambda (key value)
	       ;; The only integers in this hash table are unique IDs 
	       (when (integerp key)
		 (destructuring-bind (object timestamp) 
		     value
		   (when (< timestamp purge-cutoff)
		     (list key object) 
		     ))))
	     ht
	     )))
      ;; For each expired entry
      ;; Remove the object -> unique-id mapping 
      ;; Remove the unique-id -> (object timestamp) mapping
      (loop for (unique-id object) in expired-ids-and-objects do
	   (remhash unique-id ht)
	   (remhash object ht)))))

(defun enter-object-into-or-update-de-table (object)
  ;; See if OBJECT is already in our hash table.

  (with-lock (*object-hash-lock*)
    (let ((unique-id (gethash object *de-table*)))
      ;; If not, create a unique-id for it
      (unless unique-id
	;; This number will be unique with respect to this hash table
	;; for this process.
	(setq unique-id (next-unique-id))
	(setf (gethash object *de-table*) unique-id))

      (update-timestamp *de-table* unique-id object)
      unique-id)))

(defun remove-object-from-de-table (object)
  (with-lock (*object-hash-lock*)
    (let ((unique-id (gethash object *de-table*)))
      (if unique-id
	  (progn
	    (remhash unique-id *de-table*)
	    (remhash object *de-table*)
	    t)
	  nil))))

(defun enter-url-into-pid-table (url)
  (with-lock (*object-hash-lock*)
    (let ((unique-id (next-unique-id)))
      (update-timestamp *pid-table* unique-id url)
      unique-id)))

(defun purge-pid-table (seconds-ago)
  (with-lock (*object-hash-lock*)
    (let* ((now (get-universal-time))
	   (purge-cutoff (- now seconds-ago))
	   ;; Find all the expired timestamps and make a list of the
	   ;; keys associated with them
	   ;; and get their associated objects and their keys which are 
	   ;; the objects unique-ids.
	   (keys
	    (lmaphashnn
	     (lambda (key value)
	       (let ((timestamp (second value)))
		 (when (< timestamp purge-cutoff) key)))
	     *pid-table*)))
      (loop for key in keys do (remhash key *pid-table*)))))

(defun url-from-pid-table (unique-id)
  (with-lock (*object-hash-lock*)
    (gethash unique-id *pid-table*)))


;;; These two functions are always called under the protection
;;; of *object-hash-lock*

(defun update-timestamp (hashtable unique-id object)
  (setf (gethash unique-id hashtable) (list object (get-universal-time))))
  
(defun next-unique-id () (incf *de-unique-counter*))
