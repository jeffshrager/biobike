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

;;; Author:  JP Massar

(cformatt "*** Thread.lisp commented out for new frame system currently.")
#|
(defun thread-kdbs ()
  (thread-kegg-with-other-kdbs))

(defun thread-kegg-with-other-kdbs ()
  (when *kdb-verbose* (cformatt "Threading KEGG<->GO..."))
  (thread-kegg-compounds)
  (when *kdb-verbose* (cformatt "KEGG<->GO threading complete...")))

(defun thread-kegg-compounds ()
  (let ((kegg-name-table (make-string-equal-hash-table))
	(kegg-names nil)
        (match-count 0))
    (declare (fixnum match-count))
    ;; set up tables
    (when *kdb-verbose* (cformatt "  Processing Kegg compound names..."))
    (loop for kcf in (slotv #$KEGG.Compound #$allsubclasses)
          for j fixnum from 0 do
          (when (and *kdb-verbose* (zerop (mod j 1000))) (formatt "."))
          (dolist (name (slotv kcf #$KEGG.name.s))
	    (setf (gethash name kegg-name-table) kcf)
	    (push (cons kcf (compile-word name)) kegg-names)))
    (when *kdb-verbose* 
      (terpri)
      (cformatt "  ~D Kegg compounds word homologized." (length kegg-names))
      (cformatt "  Processing Go molecule names for matches..."))
    (loop for mf in (slotv #$GO.Molecule #$allsubclasses)
          for j fixnum from 0 do
          (when (and *kdb-verbose* (zerop (mod j 1000))) (formatt "."))
          (let* ((name (subseq (slotv mf #$fname) 4))
                 (top-match
	          (or (gethash name kegg-name-table)
	              (let ((wh (word-homology-fast name kegg-names 1 #'cdr)))
	                (when (> (car (car wh)) .8) 
                          (car (cadr (car wh)))
                          )))))
            (when top-match 
              (incf match-count)
              (setf (slotv mf #$GO.KEGGFrame.s) (list top-match))
              )))
    (when *kdb-verbose* 
      (terpri)
      (cformatt 
       "~D matches between Go Molecules and Kegg Compounds found." match-count)
      )))
|#