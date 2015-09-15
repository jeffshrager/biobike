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

;;; Author:  JP Massar.

(defparameter *configuration-variables* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun config-varname (c) (first c))
  (defun config-belongs-to (c) (second c))
  (defun config-vartype (c) (third c))
  (defun config-doc (c) (fourth c))
  (defun make-configvar (&key varname belongs-to vartype doc)
    (list varname belongs-to vartype doc))
    
  (defun configure-parameter (name belongs-to vartype doc)
    (export name :cl-user)
    (setq *configuration-variables*
          (delete name *configuration-variables* :key 'config-varname))
    (setq *configuration-variables*
          (nconc *configuration-variables* 
                 (list (make-configvar
                        :varname name :belongs-to belongs-to
                        :vartype vartype :doc doc
                        ))))))

(defmacro defcfgvar 
          (name 
           value 
           &key
           (belongs-to :weblistener)
           (parameter-type :standard)
           (doc "")
           )
  `(progn
     (defvar ,name ,value ,@(when doc (list doc)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (configure-parameter ',name ,belongs-to ,parameter-type ,doc)
       )))


(defun variable-to-display-string (v)
  (string-upcase
   (substitute #\Space #\- (string-trim "*" (symbol-name v)))))

(defun compare-namestrings (n1 n2)
  (ecase (os?)
    (:unix (string= n1 n2))
    (:windows (string-equal (substitute #\/ #\\ n1) (substitute #\/ #\\ n2)))
    ))

(defun display-configuration-variables (which-are-part-of)
  (format t "~%~%CONFIGURATION VARIABLES FOR ~A~%~%" which-are-part-of)
  (when (not (listp which-are-part-of))
    (setq which-are-part-of (list which-are-part-of)))
  (dolist (cv *configuration-variables*)
    (let* ((vn (config-varname cv))
           (bt (config-belongs-to cv))
           (vt (config-vartype cv))
           (value (symbol-value vn))
           (ds (variable-to-display-string vn)))
      (when (member bt which-are-part-of)
        (ecase vt
          (:directory
           (let ((lp (get vn :logical-pathname)))
             (cond
              ((and lp (not value))
               (error "Config var ~S has :logical-pathname ~A but no value!"
                      cv lp))
              ((and lp value)
               (let ((lpv (namestring (cl-user:translate-simple-lp lp))))
                 (unless (compare-namestrings lpv (namestring value))
                   (error "Config var ~A has translated logical path value ~S~
                           and symbol value ~S, which are inconsistent!"
                          vn lpv (namestring value)
                          )))))
             (cond
              (lp (format t ";; ~A (~A): ~A~%" ds lp value))
              (value (format t ";; ~A: ~A~%" ds value))
              (t (format t ";; ~A: <No directory specified>~%" ds value))
              )))
          (:standard (format t ";; ~A: ~S~%" ds value))
          ))))
  (terpri) (force-output)
  )

(defun describe-configuration-variables ()
  "Pretty print the name and value of each configuration variable."
  (format t "~%~%SYSTEM CONFIGURATION VARIABLES~%~%")
  (let* ((max (loop for c in *configuration-variables*
                    maximizing (length (string (config-varname c))) into z
                    finally (return z)
                    ))
         (format-string (format nil "~~~DA  ~~S~~%" max)))
    (loop for c in *configuration-variables* 
          as name = (config-varname c)
          as value = (symbol-value name) do
          (format t format-string name value)
          )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(describe-configuration-variables) :cl-user))