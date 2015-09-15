;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :user)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; SEE HOW TO DOC for usage notes!

;;; !!! THIS CODE MAY HAVE TO BE CHANGED DEPENDING UPON CHANGES IN THE 
;;;     GO XML STRUCTURE !!!

(defun strip-xml-parsed-go (xmlgo)
  (with-open-file (o "godb.lisp" :direction :output :if-exists :supersede)
    (loop for entry in (nthcdr 6 (second xmlgo))
	when (and (listp entry)
		  (eq '|term| (car entry)))
	do 
	  (let ((xlation (lispify-xml-go-entry entry)))
	    (when (numberp (car xlation))
	      (print xlation o))))
    ))

(defun lispify-xml-go-entry (entry)
  (let ((a (simple-xml-go->lisp (make-associfyable (simplify-xml-go-entry entry)))))
    (cons (car a) (remove nil (compose-dbxrefs (cdr a))))))

;;; Turns the XML crud (e.g., (foo "" (bar ...) "" frob "" (waka ...))) to 
;;; ((bar ...) (waka ...)) == remove-if-not listp....

(defun make-associfyable (entry)
  (loop for entry in entry when (listp entry) collect entry))

;;; The DBXrefs are spread all over the place, and should all be bound
;;; together, and the ECRef needs to be turned into this form: (EC-Ref
;;; (REFERENCE "1.1.1.27"))

(defun compose-dbxrefs (l)
  (append 
   ;; Remove all existing DBXRefs
   (loop for entry in l 
	 unless (eq (car entry) 'dbxref)
	 collect entry)
   ;; Find and add the EC-Ref is it's there.
   (loop for entry in l
	 when (and (eq (car entry) 'dbxref)
		   (equalp "EC" (second entry)))
	 do (return `((EC-Ref (REFERENCE ,(third entry))))))
   ;; Finally, unify all the rest of the DBXrefs
   (let ((xrefs (loop for entry in l
		      when (eq (car entry) 'dbxref)
		      collect (cdr entry))))
     (when xrefs (list (cons 'dbxrefs xrefs))))
   ))

(defun simple-xml-go->lisp (l)
  (let ((def (make-associfyable (assoc 'def l))))
    `(
      ;; Turn ID into a number (it's now: GO:000012)
      ,(parse-integer (subseq (second (assoc 'id l)) 3))
      ,(assoc 'name l)
      ,(assoc 'namespace l)
      ;; Change DEFSTR to DEFINITION for back compatibility:
      ,(cons 'definition (cdr (assoc 'defstr def)))
      ;; Take some relevant parts out directly:
      ,@(remove-if-not (lambda (entry) (member (car entry) '(is_a) :test #'eq)) l)
      ;; Change RELATIONSHIPS to associfyable things (usually just PART_OF)
      ,@(mapcar #'simplify-relationships (remove-if-not (lambda (entry) (member (car entry) '(relationship) :test #'eq)) l))
      ;; Analyze database references.
      ,@(mapcar #'passify-dbxref (remove-if-not (lambda (entry) (eq (car entry) 'dbxref)) def))
      )))

;;; For the moment only handles relationships of this form:
;;;  (RELATIONSHIP "" (TYPE "part_of") "" (TO "GO:0000161") "")
(defun simplify-relationships (r)
  `(,(read-from-string (second (third r)))
    ,(second (fifth r))))

(defun passify-dbxref (l)
  (let ((l2 (make-associfyable l)))
    `(dbxref
       ,(second (assoc 'dbname l2))
       ,(second (assoc 'acc l2)))))

(defun simplify-xml-go-entry (entry)
  (cond ((null entry) nil)
	((symbolp entry) 
	 ;; This looks weird, but it avoids string consing.
	 (case entry
	   (|accession| 'accession)
	   (|name| 'name)
	   (|definition| 'definition)
	   (|is_a| 'is_a)
	   ;; (|part_of| 'partof) -- now handled by XML <relationships>
	   (|resource| 'resource)
	   (t (read-from-string (symbol-name entry)))
	   ))
	((listp entry)
	 (cons (simplify-xml-go-entry (car entry))
	       (simplify-xml-go-entry (cdr entry))))
	((stringp entry)
	 (cond ((and (= (length entry) 41)
		     (char-equal #\: (aref entry 4))
		     (char-equal #\: (aref entry 33)))
		(read-from-string (subseq entry 34)))
	       (t (string-trim '(#\space #\newline #\return #\tab) entry))))
	(t entry)))


