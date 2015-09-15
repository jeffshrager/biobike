;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)

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

;;; Author:  JP Massar, Mark Slupesky.   

(DEFPARAMETER *bbldf-files* NIL)

(IF (NOT (EQUAL cl-user::*application-instance* :BIKE))
    (SETF *bbldf-files*
          '("arithmetic-stats-trig-doc"
            "define-doc"
            "gene-protein-doc"
            "genome-microarray-doc"
            "input-output-doc"
            "list-tables-doc"
            "logic-comparison-doc"
            "other-doc"
            "string-sequence-doc"
            "df-a-doc"
            "df-b-doc"
            "df-c-doc"
            "df-d-doc"
            "df-e-h-doc"
            "df-i-m-doc"
            "df-n-doc"
            "df-o-r-doc"
            "df-s-doc"
            "df-t-w-doc"
            ))
  (SETF *bbldf-files*
        (MAPCAR (LAMBDA (n) (bbi::S+ "reduced/" n))
                '("arithmetic-stats-trig-doc"   
                  "definition-assign-other-doc"
                  "input-output-doc"
                  "list-tables-doc"
                  "logic-comparison-doc"
                  "string-sequence-doc"
             
                  ))))

   (IF (EQUAL cl-user::*blast-lookup-database* :SEED)
       (SETQ *bbldf-files*
             (APPEND *bbldf-files* (LIST "annotation-doc"))))
			 
(load-system* "websrc:Doc;bbldf;" *bbldf-files*)

(when (fboundp 'provides) (funcall 'provides :bbldf))
