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

;;; Author:  JP Massar.


(defun frame-designators-equivalent? (fd1 fd2 &optional (on-error :error))
  #.(one-string-nl
     "Tests whether two frame designators would have the same frame name"
     "if they were converted to frames, but does not create any new frames."
     "If either of the presumed designators is neither a symbol, a string nor"
     "a frame then ON-ERROR controls what happens:  :ERROR signals an error,"
     ":WARN issues a warning and returns NIL, NIL causes NIL to be returned"
     "and any other value is erroneous.")
  #.(optimization-declaration)
  (cond
   ((symbolp fd1) (frame-designators-equivalent? (string fd1) fd2 on-error))
   ((symbolp fd2) (frame-designators-equivalent? fd1 (string fd2) on-error))
   ((and (isframe? fd1) (isframe? fd2)) (eq fd1 fd2))
   ((and (stringp fd1) (stringp fd2)) (string-equal fd1 fd2))
   ((and (stringp fd1) (isframe? fd2)) (string-equal fd1 (#^Fname fd2)))
   ((and (isframe? fd1) (stringp fd2)) (string-equal (#^Fname fd1) fd2))
   (t
    (ecase on-error
      (:error (error "Unknown frame designators ~A and/or ~A" fd1 fd2))
      (:warn (warn "Invalid frame designators ~A and/or ~A, returning NIL"
                   fd1 fd2) nil)
      ((nil) nil)
      ))))

    
;;; Return a new table restricted to a those rows which have as the value
;;; of the GENE-COLUMN-NAME column an element of GENES (or all rows if 
;;; GENES is nil) and for which the data vector satisfies DATA-PREDICATE.

;;; Each element of GENES can either be a string representing
;;; a gene name and key, or a FRAME whose #$Fname represents
;;; the gene name and key.

(defun restricted-gene-table 
       (table 
        &key
        (genes nil)
        (gene-column-name "geneID")
        (data-predicate #'identity))
  #.(one-string-nl
     "Creates a new table of rows from the old table whose data values "
     "satisfy DATA-PREDICATE and whose value in the GENE-COLUMN-NAME "
     "column (by default 'geneID') is an element of GENES, or all values"
     "if GENES is NIL")
  ;; Which rows satisfy DATA-PREDICATE?
  (let ((selected-rows
         (table-data-select 
          table data-predicate :from :data :return :row-indices)))
    (cond
     ((null genes)
      ;; If no further restriction, create a table containing those rows.
      (table-data-rows-table table selected-rows))
     (t
      ;; Otherwise remove from the list of row indices those rows that
      ;; do not match GENES, and then create the table.
      (table-data-rows-table 
       table
       (remove-if-not
        #'(lambda (row)
            (let ((gene-in-table 
                   (table-data-element table row gene-column-name)))
              (some
               #'(lambda (gene-in-list) 
                   (frame-designators-equivalent? gene-in-table gene-in-list))
               genes
               )))
        selected-rows
        ))))))

