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

#| ######################################################################

Micro-loader for Lisp systems

Copyright 1996 Michael Travers

Author: Michael Travers (mt@media.mit.edu)
Modified by JP Massar for BioLingua 2003-2004.

-------------------------------------------------------------------------

The smallest possible system loader.  Example of use:

(load-system 
 "c:/Lispcode/Mysource/"
 '("file-1"
   "file-2"
   ...
   ))

###################################################################### |#

;;; Micro-loader

(defvar *force-load-system*-recompile* nil)

(defun path->lisp-physical-pathname (path)
  "Converts any legitimate path (including a slp) to a physical pathname."
  (handler-case
      (cl-user:translate-simple-lp path)
    (error () (translate-logical-pathname path))))

;;; load-system is taken in ACL, and causes problems
(defun load-system* (base-path file-list)
  (setq base-path (namestring (path->lisp-physical-pathname base-path)))
  (with-compilation-unit ()
    (mapc (lambda (file)
            (let ((full-file (concatenate 'string base-path file)))
              (compile/load full-file)))
          file-list)))


;;; Corman Lisp does not work correctly when source files are
;;; turned into binary files (sometimes).  (But Corman compiles all the
;;; forms that it loads, so it is not necessary to COMPILE-FILE source
;;; files.)

(defun compile/load 
       (file &optional (recompile? *force-load-system*-recompile*))
  #+:CORMANLISP
  (load (source-file* file) :verbose t)
  #-CORMANLISP
  (let ((source (source-file* file))
        (fasl (fasl-file file)))
    (when (or recompile?
	      (not (probe-file fasl))
	      (> (file-write-date source)
		 (or (file-write-date fasl) 0)))
      (compile-file source :output-file fasl #+:SBCL :verbose #+:SBCL nil))
    (load fasl)))

;;; Different conventions (also sometimes CL in Allegro-land, sigh)
;;; This will go to simply "lisp" for all implementations once we standardize.

(defparameter *lisp-extension* 
    (block nil
      #+(OR MCL :ALLEGRO :LISPWORKS :SBCL :CORMANLISP) (return "lisp")
      (error "Unknown Lisp.  Need to specify a source extension.")))

(defun change-file-type (file type)
  #+:CORMANLISP
  ;; We are working around a bug in Corman's MAKE-PATHNAME 
  ;; function here.  MAKE-PATHNAME insists on a real pathname as 
  ;; the argument to :defaults, and it cannot be a logical pathname.
  (let ((p (pathname file)))
    (if (lp::logical-pathnamep p)
	(change-file-type (translate-logical-pathname file) type)
      (make-pathname :defaults p :type type)
      ))
  #-:CORMANLISP
  (make-pathname :defaults (pathname file) :type type))

(defun source-file* (file)
  (change-file-type file *lisp-extension*))

(defun fasl-file (file)
  (let ((fasl-extension
	 (block nil
	   #+(AND MCL POWERPC) (return "pfsl")
	   #+(AND MCL (NOT POWERPC)) (return "fasl")
	   #+:ALLEGRO (return "fasl")
	   #+:SBCL (return "fasl")
	   #+:LISPWORKS (return "fsl")
	   #+:CORMANLISP (return "fasl")
	   (error "Unknown Lisp.  Need to specify a binary extension.")
	   )))
    (change-file-type file fasl-extension)
    ))


