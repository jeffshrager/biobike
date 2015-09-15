;;; -*- Package: user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  (require :pxml))

(defun wb:parse-xml (string) (net.xml.parser::parse-xml string))

(defun wb:web-page-contents (url &rest keys-and-values)
  "Return the contents of (e.g., the HTML) for a web page as a string"
  (apply 'net.aserve.client:do-http-request url keys-and-values))

(defmacro wb:with-open-web-page ((stream url) &body body)
;;;  #+:MSWINDOWS
;;;  `(cg::with-open-web-page (,stream ,url) ,@body)
;;;  #-:MSWINDOWS
  `(with-input-from-string 
       (,stream (wb:web-page-contents ,url))
     ,@body
     ))

;; Make it easy to do an IN-PACKAGE...
;; And just in case CG-USER isn't defined in some version of Allegro.

(when (find-package :cg-user)
  (eval
   (read-from-string
    "(unless (fboundp 'cg-user::ip)
       (defmacro cg-user::ip (package-designator) 
         `(in-package ,package-designator)))"
    )))


(defun write-stack-trace (place &key (count t) (show-all t) (level :moderate))
  (with-standard-io-syntax
    (let ((*print-readably* nil)
	  (*print-miser-width* 40)
	  (*print-pretty* t)
	  (tpl:*zoom-print-circle* t)
	  (tpl:*zoom-print-level* nil)
	  (tpl:*zoom-print-length* nil))
      (ignore-errors			;prevent recursion
       (flet ((zoom (s)
		(let ((*terminal-io* s)
		      (*standard-output* s))
		  (apply #'tpl:do-command "zoom"
		    :from-read-eval-print-loop nil
		    :count count ; Number of rames
		    (list :all show-all level t)))))
	 (if (streamp place)
	     (zoom place)
	   (with-open-file (s place :direction :output :if-exists :supersede)
	     (zoom s))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow (list 'run-shell-command))
  )

(defun cl-user::run-shell-command (command  &rest args &key &allow-other-keys)
  (apply 'excl:run-shell-command command args))

(defun generic-copy-file (from to)
  (sys::copy-file from to))
  
(defun environment-variable-value (string)
  (sys::getenv string))