;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

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

;;; Author:  JP Massar.

;;;;  See specification in DOC/TABLE-DATA.TXT.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *table-data-api-symbols*
    '(
      simple-read-table
      simple-write-table
      read-table-data
      describe-table-data
      table-data-documentation
      table-data-headers
      table-data-header
      table-data-post-header-info
      table-data-keycols
      table-data-nrows
      table-data-ncols 
      table-data-ndatacols
      table-data-nothercols
      table-data-element
      table-data-elements
      table-data-data-row
      table-data-data-col
      table-data-data-rows
      table-data-data-cols
      table-key
      table-keys
      table-data-key-present?
      table-data-key-row-indices
      table-data-data-array
      table-data-data-subarray
      table-data-row
      table-data-col
      table-data-rows
      table-data-cols
      table-data-row-table
      table-data-rows-table
      table-data-col-table
      table-data-cols-table
      table-data-subtable
      table-data-select
      write-table-data
      ;; Not implemented below here
      table-data-select-data
      create-table-data
      compile-table-data
      write-compiled-table-data
      read-compiled-table-data
      )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (export *table-data-api-symbols* (find-package :bioutils))
    ))

#-:BIOLISP
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (defmacro one-string (&rest args) `(concatenate 'string ,@args))
    (defmacro one-string-nl (&rest args) `(concatenate 'string ,@args))
    ))
