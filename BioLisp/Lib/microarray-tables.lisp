;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 
   '(
     read-microarray-table
     )))

(defun read-microarray-table 
       (file &rest keyargs &key (organism nil) (table-format :default))
  #.(one-string-nl
     "Reads a table of microarray data from FILE and returns a TABLE-DATA"
     "object. The data is assumed to be in a standard format:"
     "The first column names a gene, and the rest of the columns are"
     "are data points. If ORGANISM is provided the gene name is prefixed"
     "with the organism prefix when the name is stored in the table."
     "Any missing values in the microarray table file are replaced with"
     "the completely arbitrary value -100.0")
  (case table-format
    (:default
     (apply
      'read-table-data 
      file
      :n-predata-fields 1 
      :key-columns '(0)
      :missing-value -100.0
      :other-rdrfuncs
      (lambda (name) 
        (if (null organism)
            (frame-fnamed name t)
          (frame-fnamed (one-string (#^organism-prefix organism) name) t)))
      keyargs
      ))
    (otherwise
     (format t "~&;; Unimplemented table format: ~A~%" table-format)
     (format t ";; Calling READ-TABLE-DATA and hoping for the best...~%")
     (apply 'read-table-data file keyargs)
     )))


      
