;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Beginnings of code to integrate Cyanobase Ocelot model and GO.

(defvar *ocelot-top* nil)
(defvar *dont-label-children-of-these-frames*)
(defvar *o* t)

(defvar *ocelot-top*)
(defun make-ocelot-frames ()
  (unless *ocelot-top*
	  ;; Load up the PCC6803 ocelot model.
    (setq *all-frames* nil)		; ************ Mike: Can I do this or does it mess up the frame system? *************
    ;; mt: might mess up some finding functions, nothing fundamental.
    ;; These need to be recreated (initially bogusly created on load!)
    (setq *ocelot-top* (make-frame* 'ocelot::frames))
    (setf (get 'ocelot::frames :oframe) (frame-named 'ocelot::frames))
    (read-ocelot-file "biol:data;cyano6803base.ocelot")
    ))

(defun cygone (&aux (outfile "biol:data;cygone.lisp"))
  (make-ocelot-frames)
  ;; Now load the GO.
  (make-go-frames)
  ;; Crawl the entire Ocelot hierarchy, and anytime anything has common names,
  ;; use word homology on the GO frames to figure out which frames this is 
  ;; likely to bind to.
  (format t "Compiling GO frame names.~%")
  (format t "Matching Ocelot frames with Common Names to GO frames; Output to: ~a~%" outfile)
  (cond ((probe-file outfile)
	 (load-existing-cygone-database outfile))
	(t (setq *dont-label-children-of-these-frames* 
		 (list (find-frame "Compounds")))
	   (with-open-file (*o* outfile :direction :output :if-exists :supersede)
			   (cygo-frame-and-children *ocelot-top*))))
  )

;;; Upon loading the existing cygone database, we do some simple filtering
;;; by deleting anything with a match score < *min-cygone-match*

(defparameter *min-cygone-match* 0.6)

(defslot ocelot-bindings)

(defun load-existing-cygone-database (infile)
  (format t "Loading existing CyGOne DB from: ~a~%(You'll have to delete this file to have the database recreated!)~%" infile)
  (with-open-file (i infile)
    (loop as record = (read i nil nil)
	  until (null record)
	  with count = 0
	  with bindings = nil
	  as (ocelot-frame-name common-names . matches) = record
	  as ocelot-frame = (find-frame ocelot-frame-name)
	  do 
	  (incf count)
	  (when (zerop (mod count 100))
		(print count))
	  (setf (slotv ocelot-frame 'go-bindings)
		(setq bindings
		   (loop for entry in matches
			 as (score . go-frame-name) = entry
			 do (rplacd entry (find-go-frame go-frame-name))
			 when (>= score *min-cygone-match*)
		       collect entry))) 
	  ;;; Here's the tricky part: We
	  ;; REVERSE the process, assigning the OCELOT frames to the
	  ;; GO frames as well!  Since the BINDINGS contain the GO
	  ;; frames, we can just walk through them.
	  (loop for entry in bindings
		as (score . go-frame) = entry
		do 
		(slotv go-frame 'ocelot-bindings) ; Needed I think to create the slot!
		(push (cons score ocelot-frame)
		      (slotv go-frame 'ocelot-bindings)))
	  finally (format t "~a Ocelot frames were assigned GO frame annotations. (and v.v.)~%" count)
	  )))

(defslot go-bindings)

(defun cygo-frame-and-children (frame)
  (recursive-descent frame (slot-accessor 'ocelot::children) #'cygo-frame))

;;; (I dislike this style...it should generate results as Lisp and then dump them.  -- Mike)

(defun cygo-frame (frame &aux (k 0))
  (let ((common-names (slotv frame 'common-name)))
    (when (and common-names
            (okay-to-compute-go-labels-for-ocelot-frame? frame))
      (when (zerop (mod (incf k) 1000)) (print k))
      ;;(describe-frame frame)
      (loop for name in common-names do
            (format *o* "~%(~%")
            (print (slotv frame ':name) *o*)
            (print (slotv frame 'common-name) *o*)
            (push (loop for (score frame) in (word-homology-fast name *go-frames* 3 (slot-accessor :wh-code))
                      as result = (cons score frame)
                      do (print (cons score (slotv frame :name)) *o*)
                      collect result)
              (slotv frame 'go-bindings))
            (format *o* "~%)~%")
            ))
    ))




;;; +++ could be more efficient; should encapsulate is-ancestor?
(defun okay-to-compute-go-labels-for-ocelot-frame? (frame)
  (loop for parent in (compute-transitive-slot frame 'ocelot::parents)
	when (member parent *dont-label-children-of-these-frames*)
	do (return nil)
	finally (return t)))

; (trace cygo-frame-and-children)
; (cygo-frame-and-children (find-frame "EC-Reactions"))
; (fdframes "EC-6")

