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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *acache-binary-file*
    (cond
     ((and (null cl-user::*acache-fasl-dir*) (null cl-user::*acache-version*))
      nil)
     ((null cl-user::*acache-fasl-dir*)
      (make-pathname :name cl-user::*acache-version* :type "fasl") 
      )
     ((null cl-user::*acache-version*)
      (format 
       t 
       ";; *** Ignoring *acache-fasl-dir* because *acache-version* is NIL!")
      nil)
     (t 
      (merge-pathnames
       (make-pathname :name cl-user::*acache-version* :type "fasl")
       cl-user::*acache-fasl-dir*
       ))))
  
  (defvar *acache-loaded* nil)
  (defvar *acache-running* nil)
  
  (require :aserve)

  (flet ((load-acache ()
           (if (null *acache-binary-file*) 
               (progn 
                 (require :acache) 
                 (format t ";; Loaded acache code from default."))
             (progn 
               (require :acache *acache-binary-file*)
               (format t ";; Loaded acache code from ~A~%" *acache-binary-file*)
               ))
           (setq *acache-loaded* t)
           (pushnew :acache *features*)))
    (handler-case
        (ecase *acache-frame-system-mode*
          (:acache (load-acache))
          (:pseudo-acache 
           (format t "Using Psuedo-acache frame system (no preloaded frames)~%")
           nil)
          (:ask (when (yes-or-no-p "Use real acache?") (load-acache))))
      (error 
       (c)
       (format t ";; Could not load Allegrocache: ~A~%" c)
       (format t ";; Assuming Allegrocache does not exist!~%")
       (format t ";; Using Pseudo-acache frame system (no preloaded frames)~%")
       ))

    ))

(defmacro not-implemented (&rest names)
  `(progn 
     ,@(loop for name in names collect
             `(defun ,name (&rest args) (error "~A not implemented." ',name))
             )))
