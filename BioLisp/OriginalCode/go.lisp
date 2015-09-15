;;; Copyright (c) 2002-2003 by Jeff Shrager and Mike Travers; All rights reserved.

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(defun walk-isas (from)
  (loop for (property value) in (cdr (assoc from *go*))
	when (eq 'isa property)
	do (walk-isas value)))

(defun walk-isas (from &optional (depth 0))
  (let ((entry (cdr (assoc from *go*))))
    (loop for space from 1 to (* 2 depth) do (format t " "))
    (format t "~a: ~a~%" depth (second (assoc 'name entry)))
    (loop for (property value) in entry
	  when (eq 'isa property)
	  do (walk-isas value (incf depth)))))


(defvar *go-id->plist-table* (make-hash-table :test #'equal))
(defvar *go-id->children-table* (make-hash-table :test #'equal))
(defvar *go-name->id-table* (make-hash-table :test #'equal))

(compile
(defun invert-go ()
  ;; Clear old values out of the tables in case we run this more than once
  (clrhash *go-id->plist-table*)
  (clrhash *go-id->children-table*)
  (clrhash *go-name->id-table*)
  ;; Walk through each *GO* entry...
  (loop for (id . info) in *go*
	do 
	;; Set up the main association between go ID and the property list.
	(setf (gethash id *go-id->plist-table*) info)
	;; Now go through each property and disperse them into the relevant tables.
	(loop for (prop value) in info
	      do (case prop
		       (isa (push id (gethash value *go-id->children-table*)))
		       (name (setf (gethash (string-downcase value) *go-name->id-table*) id))
		       (synonym (setf (gethash (string-downcase value) *go-name->id-table*) id))
		       )))))


(defmacro assocadr (key alist)
  `(cadr (assoc ,key ,alist)))

(defun define (name)
 (let ((id (gethash (string-downcase name) *go-name->id-table*)))
  (when id (list id (assocadr 'definition (gethash id *go-id->plist-table*))))))

(defun ancestors-of (id)
  (cons id 
	(loop for (prop value) in (gethash id *go-id->plist-table*)
	      when (eq prop 'isa)
	      collect (ancestors-of value))))

(defun go->names (id-tree &optional (depth 0))
  (cond ((null id-tree) nil)
        ((numberp id-tree)
	 (loop for i from 1 to (* 2 depth) do (format t " ")) ; space over
         (format t "~6a (~a) = ~a~%"
		 id-tree depth (assocadr 'name (gethash id-tree *go-id->plist-table*))))
	(t (mapcar #'(lambda (subtree) (go->names subtree (1+ depth))) id-tree))
	))
			  

(defun isa*? (class type)
  (or (= class type) ; If we hit the type we're after, we're done!
      (loop for (property value) in (cdr (gethash class *go-id->plist-table*))
	    when (eq 'isa property)
	    do (if (isa*? value type)
		   (return t)))))

(defmacro is? (object-name a target-type-name)
  `(isa*? (car (define ,object-name))
          (car (define ,target-type-name))))

(defun my-immediate-children (me)
  (loop for (id . plists) in *go*
	when (loop for (property value) in plists
		   when (and (eq 'isa property)
			     (= me value))
		   do (return t))
	collect id))

(defun my-immediate-children (me)
  (gethash me *go-id->children-table*))

(defun find-all-my-descendants (me)
    (cond ((null me) nil)
	  (t (cons me (mapcar #'find-all-my-descendants
			      (my-immediate-children me))))))

(defun %slot-value (id slot)
  (loop for (prop value) in (gethash id *go-id->plist-table*)
      when (eq prop slot)
      collect value))

(defun parents (id)
  (%slot-value id 'isa))

(defun superparts (id)
  (%slot-value id 'part-of))

(compile
(defun inverse-slot-function (slot-function)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (id . ignore) in *go*
	do (loop for value in (funcall slot-function id)
	       do (push id (gethash value ht))))
    #'(lambda (id) (gethash id ht)))))

(defun transitive-closure (function)
  (labels ((all (thing)
	     (cons thing
		   (remove-duplicates
		    (mapcan #'all (funcall function thing))))))
    #'all))

(defun e (id-or-list)
  (if (numberp id-or-list)
      (%slot-value id-or-list 'name)
    (mapcan #'e id-or-list)))

(defun anglicize-accessor (accessor)
  #'(lambda (english-in)
      (e (funcall accessor (un-e english-in)))))

