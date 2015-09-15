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

;;; Author:  JP Massar

;;;;  See specification in DOC/TABLE-DATA.TXT.

;;; The standard TABLE-DATA object.

(defclass table-data () 
  ((name :accessor td-name :initarg :td-name)
   (source-file :accessor td-source-file :initarg :td-source-file)
   (docs :accessor td-docs :initarg :td-docs)
   (headers :accessor td-headers :initarg :td-headers)
   (posthdrs :accessor td-posthdrs :initarg :td-posthdrs)
   (precols :accessor td-precols :initarg :td-precols)
   (datacols :accessor td-datacols :initarg :td-datacols)
   (postcols :accessor td-postcols :initarg :td-postcols)
   (othercols :accessor td-othercols :initarg :td-othercols)
   (keycols :accessor td-keycols :initarg :td-keycols)
   (rdrfuncs :accessor td-rdrfuncs :initarg :td-rdrfuncs)
   (data-rdrfunc :accessor td-data-rdrfunc :initarg :td-data-rdrfunc)
   (descs :accessor td-descs :initarg :td-descs)
   (nrows :accessor td-nrows :initarg :td-nrows)
   (ncols :accessor td-ncols :initarg :td-ncols)
   (nprecols :accessor td-nprecols :initarg :td-nprecols)
   (ndatacols :accessor td-ndatacols :initarg :td-ndatacols)
   (npostcols :accessor td-npostcols :initarg :td-npostcols)
   (nothercols :accessor td-nothercols :initarg :td-nothercols)
   (nkeycols :accessor td-nkeycols :initarg :td-nkeycols)
   (other-array :accessor td-other-array :initarg :td-other-array)
   (data-array :accessor td-data-array :initarg :td-data-array)
   (data-type :accessor td-data-type :initarg :td-data-type)
   (hashes :accessor td-hashes :initarg :td-hashes)
   (missing-count :accessor td-missing-count :initarg :td-missing-count)
   (missing-value :accessor td-missing-value :initarg :td-missing-value)
   ))


(defstruct table-key 
  value rows column-index column-designator table complete?)

(defvar *table-filename*)
(defvar *table-file-line-count*)
(defparameter *warn-on-data-coercion* nil)

;;; Create a readable error message spanning multiple lines.

(defmacro lerror (string &rest args)
  (dolist (arg args)
    (if (stringp arg) 
        (progn (pop args) (setq string (concatenate 'string string " " arg)))
      (return)))
  `(error ,string ,@args))

