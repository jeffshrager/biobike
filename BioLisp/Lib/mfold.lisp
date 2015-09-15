;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2006 by The BioBike Team                             s    |
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

(defvar *mfold-bin-dir* 
  (cl-user::translate-simple-lp "biotools:mfold;current;bin;"))
(defvar *mfold-dat-dir* 
  (cl-user::translate-simple-lp "biotools:mfold;current;dat;"))

;;;  mfold uses all sorts of globals and input files, and creates all
;;; sorts of output files with wako names!

(defun run-mfold (seq)
  #.(one-string-nl
     "DOCUMENT ME!"
     )
  (with-temp-directory (dir "tmp:")
    (with-temp-file-in (infile dir :name "seq")
      (with-open-file (infile-stream (print infile) :direction :output)
        (format infile-stream ">seq~%~a~%" seq))
      (list-file-contents infile)
      (let ((mfold-command 
             (formatn 
              (one-string 
               "MFOLDBIN=~a; export MFOLDBIN; MFOLDDAT=~a; export MFOLDDAT; "
               "MFOLDLIB=~a; export MFOLDLIB; PATH=$PATH:$MFOLDBIN; "
               "export PATH; ~a/mfold SEQ=~a.temp RUN_TYPE=text")
              *mfold-bin-dir* *mfold-dat-dir* *mfold-dat-dir* *mfold-bin-dir*
              (pathname-name infile))))
        (case (protected-shell-command
               mfold-command :directory (namestring dir) :exec? nil)
          (:timeout nil)
          (otherwise 
           (loop for file in 
                 (directory (merge-pathnames (directory-namestring infile) "*.*"))
                 collect (list (namestring file)
                               (file-to-string-list file)))))))))
