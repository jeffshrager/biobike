;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-user; -*-


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

;;;; *****>  THIS FILE BELONGS IN THE TOPLEVEL CLOCC DIRECTORY  <*****

;;; Provides a mechanism to load the files needed to access the XML
;;; functionality within CLOCC/CLLIB.

(defvar *clocc-root* 
  (namestring 
   (make-pathname
    :host (pathname-host *load-truename*)
    :device (pathname-device *load-truename*)
    :directory (pathname-directory *load-truename*)
    :name nil :type nil :version nil
    )))

(load (concatenate 'string *clocc-root* "load-generic.lisp"))

#+:lispworks
(load "clocc:src;cllib;xml.fsl")
#+:allegro
(load "clocc:src;cllib;xml.fasl")
#+:cormanlisp
(load "clocc:src;cllib;xml.lisp")
#+:sbcl
;; XXX for some reason SBCL didn't like loading the fasl output
;; it complained about multiple lisp objects in one FASL file
;; this works, i'll look into it later - mas 9/22/04
(load "clocc:src;cllib;xml.lisp")

(let* ((src (concatenate 'string *clocc-root* "xml-to-list.lisp"))
       (obj (compile-file-pathname src)))
  (when (or (not (probe-file obj))
            (<= (file-write-date obj) (file-write-date src)))
    (compile-file src))
  (load obj))

