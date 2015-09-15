;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

#| this stuff is unused
(defun thread-kegg-frame (kf)
  (when (frame-is-a kf #$KEGG.Compound)
    (vwhen (x (kegg->mol-frame kf))
	   (setf (slotv kf #$KEGG.GOframe.s)
	     (list x)))))

;(def-inverse-slot #$GO.KEGGframe.s #$KEGG.GOframe.s)

;;; maybe this should return a list?
(defun kegg->mol-frame (kcf)		;
  ;; try for an exact match
  (dolist (name (slotv kcf #$KEGG.name.s))
    (vwhen (x (find-mol-frame name))
	   (return-from kegg->mol-frame x)))
  ;; try an inexact match
  (let ((homies 
	 (mapcan #'best-wh-match
		 (slotv kcf #$KEGG.name.s))))
    ;; +++ should maximize over homies, but just take first for now
    (if (> (caar homies) .7)
	(cadar homies))))

(defun find-mol-frame (name)
  (frame-fnamed (format nil "MOL.~A" name)))


(defun best-wh-match (name)
  (word-homology-fast name (slotv #$GO.Molecule #$allsubclasses) 1 (slot-accessor #$fName)))
|#

;;; Going the other way makes more sense...

;;; Need to do this (inverses only work one way -- another thing to fix in the REAL frame system)
(def-inverse-slot #$KEGG.GOframe.s #$GO.KEGGframe.s)

(defun thread-kegg-compounds ()
  (let ((kegg-name-table (make-string-equal-hash-table))
	(kegg-names '()))
    ;; set up tables
    (dolist (kcf (slotv #$KEGG.Compound #$allsubclasses))
      (dolist (name (slotv kcf #$KEGG.name.s))
	(setf (gethash name kegg-name-table) kcf)
	(push (cons kcf (compile-word name)) kegg-names)))
    (dolist (mf (slotv #$GO.Molecule #$allsubclasses))
      (let ((name (subseq (slotv mf #$fname) 4)))
	(vwhen
	 (x (or (gethash name kegg-name-table)
	     (let ((wh (word-homology-fast name kegg-names 1 #'cdr)))
	       (if (> (car (car wh)) .8)
		   (car (cadr (car wh)))))))
	 (setf (slotv mf #$GO.KEGGFrame.s) (list x)))))))
	       
  
