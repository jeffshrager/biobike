;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

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

;;; Author: JP Massar

(eval-when (:load-toplevel :execute)

  (defvar *acache-loaded* nil)
  (defvar *acache-running* nil)
  
  (require :aserve)

  (flet ((load-acache ()
           (if (null user::*acache-file-for-require*) 
               (progn 
                 (require :acache) 
                 (format t ";; Loaded acache code from default."))
             (progn 
               (require :acache user::*acache-file-for-require*)
               (format 
                t
                ";; Loaded acache code from ~A~%" 
                user::*acache-file-for-require*)
               ))
           (setq *acache-loaded* t)
           (pushnew :acache *features*)))
    (handler-case (load-acache)
      (error 
       (c)
       (format t ";; Could not load Allegrocache: ~A~%" c)
       (error "Fatal error...cannot continue!")
       ))))

(defmacro not-implemented (&rest names)
  `(progn 
     ,@(loop for name in names collect
             `(defun ,name (&rest args) (error "~A not implemented." ',name))
             )))
