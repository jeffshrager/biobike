;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :user)
  (defpackage :biocyc (:use :wlisp :biolisp :webuser :bioutils))
  (in-package :biocyc)
  )

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

;;; Interface with Peter Karp's BioCyc PGDB creator (path-o-logic), in
;;; batch mode.  Original author: Jeff Shrager

(defun create-pgdb (orgframe)
  "Given the frame representating an organism, try to create a new Pathway Genome DataBase for it."
  (with-temp-directory (dir "tmp:" :delete? nil)
    (create-organism-params.dat orgframe dir)
    (loop for (contig-frame . contig-filename) in (create-genetic-elements.dat orgframe dir)
	  do 
	  (format t "Creating ~a~%" contig-filename)
	  (create-gene-annotations contig-frame contig-filename)
	  )
    (run-pathologic dir)
;    (analyze-pgdb orgframe) ; The pgdb gets created in the standard biocyc directories
    ))

(defun run-pathologic (dir)
  (excl::run-shell-command (print (format t "/usr/local/biotools/biocyc/pathway-tools -patho ~a" dir))))

(defun create-gene-annotations (contig-frame contig-filename)
  (with-open-file (o contig-filename :direction :output :if-exists :supersede)
    (loop for gene across (#^Genes-Sorted-by-Position contig-frame)
	  do 
	  (dump-param o "ID" (#^fname gene))
	  (dump-param o "NAME" (#^fname gene))
	  (dump-param o "STARTBASE" (#^from gene))
	  (dump-param o "ENDBASE" (#^to gene))
	  (dump-param o "FUNCTION" (#^annotation gene))
	  (dump-param o "PRODUCT-TYPE" "P")
	  (format o "//~%")
	  )))

(defun dump-param (stream name value)
  (format stream "~a~c~a~%" name #\tab value))

(defun create-organism-params.dat (orgframe dir)
  ;; There are lots of optional fields, but only ID and NAME are required.  See 
  ;; UserGuide2.pdf, section 1.6.1 for more info.
  (with-open-file (o (merge-pathnames dir "organism-params.dat") :direction :output :if-exists :supersede)
    (dump-param o "ID" (#^fName orgframe))
    (dump-param o "STORAGE" "FILE")
    (dump-param o "NAME" (first (#^ALTERNATIVE-NAMES orgframe)))
    ))

(defun create-genetic-elements.dat (orgframe dir)
  ;; See sample file here: http://bioinformatics.ai.sri.com/ptools/sample-genetic-elements.dat
  ;; This wants to return to the caller a list: (contig-frame . contig-filename)
  (with-open-file (o (merge-pathnames dir "genetic-elements.dat") :direction :output :if-exists :supersede)
      (loop for contig in (#^Contiguous-Sequences orgframe)
	    as contig-name = (#^fname contig)
	    as contig-file-path = (format nil "~a.pf" (merge-pathnames contig-name dir))
	    do 
	    (dump-param o "ID" contig-name)
	    (dump-param o "TYPE" ":PLASMID")
	    (dump-param o "CIRCULAR?" "T")
	    (dump-param o "ANNOT-FILE" contig-file-path) ; !!! Need to be the full pathname !!!
	    (format o "//~%")
	    collect (cons contig contig-file-path))))

