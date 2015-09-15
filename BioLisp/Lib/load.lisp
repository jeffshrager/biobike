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

(defparameter *biolisp-library-files*

  '(
    "library-api"
    "orthologs"
    "organism-gene-frames"
    "uberprims"
    "blast"
    "meme"
    "clustal"
    "phylip"
    "rnaz"
    "rtable"
    "crossmetrics"
    "gnuplot"
    "microarray-tables"
    "pubmed"
    #-:sframes
    "reactome"
    #-:sframes
    "pwys"
    ))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *original-biolisp-library-files* nil))

;; Once we define this function for real, it should go into bootstrap code.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun is-dxl-image? () nil))

(defun reload-biolibrary () 
  (load-system* "biol:Lib;" *biolisp-library-files*)
  (and (fboundp 'provides) (funcall 'provides :BioLibrary)))

(cond
 ;; The list of library files may have been changed (added to)
 ;; from when this dxl image was saved.
 ;; Figure out which are the new files and load them.
 ((and (is-dxl-image?) *original-biolisp-library-files*)
  (let ((new-files 
         (set-difference 
          *original-biolisp-library-files* *biolisp-library-files*
          :test #'string=
          )))
    (if new-files
        (progn
          (format t "~&;; Loading new Bio-library modules~%")
          (dolist (new-file new-files)
            (format t ";;   Loading Bio-library file ~S~%" new-file)
            (load-system* "bio:Lib;" (list new-file)))
          (format t ";; ~D new Bio-library modules loaded~%" 
                  (length new-files)))
      (format t "~&;; No new Bio-library modules found.  None loaded.~%"))))
 ;; Simulate REQUIRES
 ((or (and (boundp '*requires-modules*)
           (not (member :BioLibrary *requires-modules*)))
      (and (boundp '*force-requires-reload*) *force-requires-reload*)
      (and (boundp '*force-load-system*-recompile*) 
           *force-load-system*-recompile*))
  (reload-biolibrary))
 ;; The LOAD of this file serves as a REQUIRES substitute, so we want
 ;; a special function to call to force a call to LOAD-SYSTEM*
 (t
  (format t "~&;; Bio-library already loaded.  To reload/recompile, call ~A~%"
          "(reload-biolibrary)")))

(setq *original-biolisp-library-files* *biolisp-library-files*)

