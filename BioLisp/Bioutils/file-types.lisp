;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author: JP Massar.

;;; These are the file types we will deal with when manipulating
;;; FASTA databases.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fasta-binary-ft*
    (pathname-type (compile-file-pathname "foo.lisp")))
  (defparameter *fasta-index-ft* "fidx")
  (defparameter *fasta-compiled-index-ft* *fasta-binary-ft*)
  (defparameter *fasta-database-ft* "fdbs")
  (defparameter *fasta-compiled-database-ft* *fasta-binary-ft*)
  )

;;; Some helper functions to deal with these file types.

(defun is-of-ft (type-from-path fasta-ft)
  (string-equal type-from-path fasta-ft))

(defun is-of-database-ft (path)
  (is-of-ft (pathname-type path) *fasta-database-ft*))

(defun pathname-of-ft (p type)
  (make-pathname
   :host (pathname-host p) 
   :device (pathname-device p)
   :directory (pathname-directory p) 
   :name (pathname-name p)
   :type type 
   :version (pathname-version p)
   ))

(defun pathname-of-database-ft (p) (pathname-of-ft p *fasta-database-ft*))
(defun pathname-of-index-ft (p) (pathname-of-ft p *fasta-index-ft*))
(defun pathname-of-compiled-index-ft (p)
  (pathname-of-ft p *fasta-compiled-index-ft*))
(defun pathname-of-compiled-database-ft (p)
  (pathname-of-ft p *fasta-compiled-database-ft*))

;;; We use this function to canonicalize filenames.  In particular,
;;; file information is stored in FASTA index and database files
;;; using this representation.

(defun true-namestring (path) (namestring (truename path)))


;;; Functions to produce compiled versions of fasta index and
;;; database files, and to create an index file if none exists.

(defun compile-silently (file &rest compile-file-keyargs)
  (let ((*compile-verbose* nil) (*compile-print* nil))
    (apply #'compile-file file compile-file-keyargs)))

(defun load-silently (file &rest load-keyargs)
  (let ((*load-verbose* nil) (*load-print* nil))
    (apply #'load file load-keyargs)))

(defun compile-with-message
    (sourcepath binpath &rest format-args)
  (when *fasta-verbose* (apply #'format t format-args))
  (compile-silently sourcepath :output-file binpath))

(defun out-of-date (base-file derived-file)
  (<= (file-write-date derived-file) (file-write-date base-file)))

;;; Compile the database file if its compiled version does
;;; not exist or was written before the database file was created/modified

(defun ensure-compiled-db-file (dbpath dbfile cpath)
  (cond
   ((null (probe-file cpath))
    (compile-with-message
     dbpath cpath
     "~&;; No compiled version of database file ~A~%~A~%" 
     dbfile
     ";; Creating compiled version."))
  ((out-of-date dbpath cpath)
   (compile-with-message
    dbpath cpath
    "~&;; Compiled version of database file ~A~% ~A~%"
    dbfile
    ";;   is out of date.  Creating new compiled version."))
  (t nil)
  ))


;;; Create an index file from a FASTA source data file if the
;;; index file does not exist or is out of date.
;;; If an index file was created, automatically generate its
;;; compiled version.
;;; If no index file was created (i.e., it already exists and is in sync)
;;; then create its compiled version if the compiled version does not
;;; exist or is out of date.

(defun ensure-compiled-fasta-index-file 
       (dbpath dbfile ipath cpath 
               &optional (key-function #'default-fasta-key-function))
  (cond
   ;; No index file.
   ((null (probe-file ipath))
    (when *fasta-verbose*
      (format t ";; Creating index file for FASTA file ~A~%" dbfile))
    (create-fasta-index-file dbpath dbfile ipath cpath key-function))
   ;; An out of date index file.
   ((out-of-date dbpath ipath)
    (when *fasta-verbose*
      (format t ";; Index file for FASTA file ~A~%~A~%" dbfile
	      ";;   is out of date.  Creating new index file."))
    (create-fasta-index-file dbpath dbfile ipath cpath key-function))
   ;; No compiled index file.
   ((null (probe-file cpath))
    (compile-with-message
     ipath cpath
     "~&;; No compiled version of index file for FASTA file ~A~%~A~%" dbfile
     ";;   Creating compiled version."))
   ;; An out of date compiled index file.
   ((out-of-date ipath cpath)
    (compile-with-message
     ipath cpath
     "~&;; Compiled version of index file for FASTA file ~A~% ~A~%" dbfile
     ";;   is out of date.  Creating new compiled version."))
   (t nil)
   ))

(defun create-fasta-index-file (dbpath dbfile ipath cpath key-function)
  (fasta-index-to-file 
   (parse-fasta-file dbpath :key-function key-function)
   :hash-table :compile? t :load? nil)
  (compile-with-message
   ipath cpath
   "~&;; Creating compiled index file for FASTA file ~A~%" dbfile))

;;; Unnecessarily general.  I wanted to be able to have compiled files
;;; end in other things that .fsl (for Lispworks), but if you try to
;;; use LOAD on a file that doesn't have type .fsl it thinks it is a
;;; source file even if it is, indeed, the result of compilation.
;;; (Allegro does this correctly)

(defun maybe-compile-to-type-and-load
    (source-filepath output-type &key (compile? t) (load? nil))
  (let ((*compile-print* nil) 
	(*compile-verbose* nil)
	(*load-print* nil)
	(*load-verbose* nil)
	(output-path (pathname-of-ft source-filepath output-type)))
    (cond 
     ((and compile? load?) 
      (load (compile-file source-filepath :output-file output-path)))
     (compile? (compile-file source-filepath :output-file output-path))
     (load? (load source-filepath))
     )))

