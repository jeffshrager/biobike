;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

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

;;; A list of COMMON LISP symbols we want to shadow when we create
;;; a user's package when the user logs in to the BioWebListener.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *user-package-lisp-functions-for-shadowing-import*
    '(:sort :stable-sort)
    ))

;;; Define the SHADOWLISP package, which contains symbols with the
;;; same name as the symbols in Common Lisp that we want to shadow.

(defpackage :shadowlisp
  (:use :common-lisp)
  #.`(:shadow ,@*user-package-lisp-functions-for-shadowing-import*)
  (:export "*SYMBOLS-TO-SHADOWING-IMPORT*"))

;;; This is the list we will use as the argument to SHADOWING-IMPORT
;;; when we call it when the user logs in to the BioWebListener,
;;; immediately after the user's package is created.  The symbols on
;;; this list are the symbols in the SHADOWLISP package whose names
;;; are the strings defined by *SYMBOL-STRINGS-FOR-SHADOWING-IMPORT*

(defparameter shadowlisp:*symbols-to-shadowing-import*
  (package-shadowing-symbols :shadowlisp))


;;; And here are the definitions of the functions we want to redefine.

(defun shadowlisp::sort (sequence predicate &key key in-place?)
  "Same as LISP:SORT except SEQUENCE is copied unless IN-PLACE? is T"
  (common-lisp:sort 
   (if in-place? sequence (copy-seq sequence)) predicate :key key))

(defun shadowlisp::stable-sort (sequence predicate &key key in-place?)
  "Same as LISP:STABLE-SORT except SEQUENCE is copied unless IN-PLACE? is T"
  (common-lisp:stable-sort 
   (if in-place? sequence (copy-seq sequence)) predicate :key key))


