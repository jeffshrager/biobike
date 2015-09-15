;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bioutils)

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

;;; Author: JP Massar.

;;;; See the API documentation in doc/fastaapi.txt
;;;; for a detailed description of the interface.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fasta-api-symbols*
    '(
      open-fasta-db
      with-fasta-db
      close-fasta-db
      create-fasta-db
      find-fasta-record
      find-fasta-subsequence
      with-fasta-db-keys
      *fasta-verbose*
      make-fasta-iter
      )))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (export *fasta-api-symbols* (find-package :bioutils))
    ))

;;; Make a simple definition for ONE-STRING and friends if the
;;; actual definition isn't loaded.

#-:BIOLISP
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (defmacro one-string (&rest args) `(concatenate 'string ,@args))
    (defmacro one-string-nl (&rest args) `(concatenate 'string ,@args))
    (defun strip-file-of-returns-preceding-newlines (file)
      (declare (ignore file)) nil)
    ))

