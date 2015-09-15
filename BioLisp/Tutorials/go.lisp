;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers and JP Massar; 
;;; All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(defvar *go* nil "Database, really defined in godb file")

(defun load-go () (load "Biol:data;godb.lisp"))
(defun load-smallgo () (load "Biol:data;smallgodb.lisp"))

;; Original version of WALK-ISAS commented out.
;; It is best to cut and paste each version from the tutorial
;; as it is presented.

#|

(defun walk-isas (from)
  (loop for (property value) in (cdr (assoc from *go*))
	when (eq 'isa property)
	do (walk-isas value)))
|#

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

(defun invert-go ()
  ;; Clear old values out of the tables in case we run this more than once
  (clrhash *go-id->plist-table*)
  (clrhash *go-id->children-table*)
  (clrhash *go-name->id-table*)
  ;; Walk through each *GO* entry...
  (loop 
   for (id . info) in *go* do
   ;; Set up the main association between go ID and the property list.
   (setf (gethash id *go-id->plist-table*) info)
   ;; Now go through each property and disperse them into the relevant tables.
   (loop 
    for (prop value) in info do
    (case prop
      (isa (push id (gethash value *go-id->children-table*)))
      (name (setf (gethash (string-downcase value) *go-name->id-table*) id))
      (synonym (setf (gethash (string-downcase value) *go-name->id-table*) id))
      ))))


(defun define (name)
 (let ((id (gethash (string-downcase name) *go-name->id-table*)))
  (when id (list id (assocadr 'definition (gethash id *go-id->plist-table*))))))

(defun ancestors-of (id)
  (cons id 
	(loop for (prop value) in (gethash id *go-id->plist-table*)
	      when (eq prop 'isa)
	      collect (ancestors-of value))))

(defun go->names (id-tree &optional (depth 0))
  (cond 
   ((null id-tree) nil)
   ((numberp id-tree)
    (loop for i from 1 to (* 2 depth) do (format t " ")) ; space over
    (format t "~6a (~a) = ~a~%"
	    id-tree depth 
	    (assocadr 'name (gethash id-tree *go-id->plist-table*))))
   (t (mapcar #'(lambda (subtree) (go->names subtree (1+ depth))) id-tree))
   )
  (values))
			  

(defun isa*? (class type)
  (or (= class type) ; If we hit the type we're after, we're done!
      (loop for (property value) in (cdr (gethash class *go-id->plist-table*))
	    when (eq 'isa property)
	    do (if (isa*? value type)
		   (return t)))))

(defmacro is? (object-name a target-type-name)
  (declare (ignore a))
  `(isa*? (car (define ,object-name))
          (car (define ,target-type-name))))


;;; You can execute (ask-sample-questions) instead of cutting and
;;; pasting each question expression from the tutorial.

(defun ask-questions (question-forms)
  (format t "~&~%")
  (dolist (q question-forms)
    (format t "Q: ~A~%" q)
    (format t "A: ~A~%~%" (if (eval q) "YES" "NO"))
    ))

(defun ask-sample-questions ()
  (ask-questions
   '((is? "nadh dehydrogenase" an "oxidoreductase")
     (is? "nadh dehydrogenase" an "atpase")
     (is? "thioredoxin reductase (NADPH)" an "antioxidant")
     (is? "atpase" a "transporter")
     (is? "nadh dehydrogenase" an "oyidoreductase")
     )))

;; Original version of MY-IMMEDIATE-CHILDREN commented out.
;; It is best to cut and paste each version from the tutorial
;; as it is presented.

#|
(defun my-immediate-children (me)
  (loop for (id . plists) in *go*
	when (loop for (property value) in plists
		   when (and (eq 'isa property)
			     (= me value))
		   do (return t))
	collect id))
|#

(defun my-immediate-children (me)
  (gethash me *go-id->children-table*))

(defun find-all-my-descendants (me)
    (cond ((null me) nil)
	  (t (cons me (mapcar #'find-all-my-descendants
			      (my-immediate-children me))))))

;;; You can execute (do-descendants-of-example) instead of 
;;; doing the four steps in the tutorial.

(defun do-descendants-of-example ()
  (let ((fcet-id (car (define "flavin-containing electron transporter"))))
    (go->names (find-all-my-descendants fcet-id))
    ))


(defun %slot-value (id slot)
  (loop for (prop value) in (gethash id *go-id->plist-table*)
      when (eq prop slot)
      collect value))

(defun parents (id)
  (%slot-value id 'isa))

(defun superparts (id)
  (%slot-value id 'part-of))

(defun inverse-slot-function (slot-function)
  ;; First create a new hash table...
  (let ((ht (make-hash-table :test #'equal)))
    ;; Now fill it up with data.
    ;; We loop over each ID in the *GO* table.  For each ID
    ;; we call the SLOT-FUNCTION on it, and get back a
    ;; list of values.  (Usually one one value is returned)
    ;; Each value is used as a KEY in the new hash table, and
    ;; ID is added as a datum for that KEY.  So for every
    ;; possible value that SLOT-FUNCTION can return, we can
    ;; now access all the ID's that have that value for that slot,
    ;; by using the hash table.  Hence we have 'inverted' 
    ;; SLOT-FUNCTION.
    (dolist (go-entry *go*)
      (let ((id (first go-entry)))
	(loop for value in (funcall slot-function id)
	       do (push id (gethash value ht)))))
    ;; Now return a function that, given a slot value V, returns
    ;; the ID's that have that value V in the slot accessed by
    ;; SLOT-FUNCTION.
    #'(lambda (v) (gethash v ht))))


;;; You can type (define-children-function) and (define-subparts-function)
;;; instead of cutting and pasting the two SETF forms in the tutorial.

;;; Children is the inverse of parents:
(defun define-children-function ()
  (setf (symbol-function 'children)
    (inverse-slot-function #'parents)))

;;; Subparts is the inverse of superparts:
(defun define-subparts-function ()
  (setf (symbol-function 'subparts)
    (inverse-slot-function #'superparts)))



(defun transitive-closure (function)
  (labels ((all (thing)
	     (cons thing
		   (remove-duplicates
                    ;; If (funcall function thing) returns NIL
                    ;; the MAPCAN becomes a noop so #'all is
                    ;; never called.  This terminates the recursion.
		    (mapcan #'all (funcall function thing))))))
    #'all))

;;; Ancestors is the transitive closure of parents:
(defun define-ancestors-function ()
  (setf (symbol-function 'ancestors)
    (transitive-closure #'parents)))

;;; Descendants is the transitive closure of children:
(defun define-descendants-function ()
  (setf (symbol-function 'descendants)
    (transitive-closure #'children)))


(defun e (id-or-list)
  (if (numberp id-or-list)
      (%slot-value id-or-list 'name)
    (mapcan #'e id-or-list)))

(defun un-e (x) 
  (declare (ignore x))
  "This is just a placeholder, you will redefine it in the tutorial"
  nil)

(defun define-un-e-function ()
  (let ((un-e-list (inverse-slot-function #'e)))
    (setf (symbol-function 'un-e)
      #'(lambda (english)
          (car (funcall un-e-list english))))))

(defun anglicize-accessor (accessor)
  #'(lambda (english-in)
      (e (funcall accessor (un-e english-in)))))

(defun define-e-descendants-function ()
  (setf (symbol-function 'e-descendants)
    (anglicize-accessor #'descendants)))
