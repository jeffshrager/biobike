;;; -*- Package: weblistener; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :weblistener) 

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defparameter *shadowed-def-form-symbols*
    '(:defun :defmacro :defconstant :defparameter :defvar)))

(cl:defparameter *shadowing-import-def-form-symbols*  ())

;;; This (theoretically, at least) makes the pretty printer treat
;;; the WORKSPACE symbols that shadow the Lisp DEF* symbols equivalently
;;; to the corresponding Lisp DEF* symbol for pretty printing (indenting)
;;; purposes.

(loop for shadowed-symbol in *shadowed-def-form-symbols* do
      (let ((lisp-symbol (find-symbol (string shadowed-symbol) :common-lisp)))
        (set-pprint-dispatch 
         `(cons (member ,shadowed-symbol)) 
         (pprint-dispatch `(,lisp-symbol) nil)
         )))

;;; In order to store definitional forms in a workspace we need to
;;; customize the macros using the mechanism defined in WLISP to
;;; capture the form.

(define-customizer standard-workspace-redefinition 
    (&whole whole &original original definer name &body body) :all
  (declare (ignore body))
  (values 
   whole 
   (when (user-session-id) 
     `((redefinition-check ',name ,(intern (symbol-name definer) :keyword))))
   (when (user-session-id) 
     `((eval-when (:load-toplevel :execute)
	 (setf
	  (get ',name ,(definition->type-of-definition definer))
          ',original
	  (get ',name ,(definition->time-of-definition definer))
          (get-universal-time)))))))

(define-customizer workspace-defun
    (&whole whole defun name (&rest args) &rest body) defun
  ;; Only go to all this trouble for user-defined stuff
  (if (null (user-session-id))
      (values whole nil nil)
      (cl:multiple-value-bind (doc-string decls body-forms bad-decls)
	  (parse-doc-decls-body body)
	(values
	 `(,defun ,name ,args 
	    ,@(when doc-string (list doc-string))
	    ;; For Allegro Linux to catch infinite recursion.
	    (declare (notinline ,name))
	    ,@(append decls bad-decls)
	    ,@body-forms)
	 nil nil))))

(cl:defun definition->type-of-definition (definer)
  (case definer
    ((cl:defvar cl:defparameter cl:defconstant) :variable-definition)
    ((cl:defun cl:defmacro) :procedure-definition)))

(cl:defun definition->time-of-definition (definer)
  (case definer
    ((cl:defvar cl:defparameter cl:defconstant) :variable-definition-time)
    ((cl:defun cl:defmacro) :procedure-definition-time)))

