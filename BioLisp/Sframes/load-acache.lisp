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

;;; Author:  JP Massar

;; use T for speed
(defvar user::*ofd-mode* :full)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; (defvar *allegrocache-server* nil)
  )

(eval-when (:load-toplevel :execute)
  (if db.ac:*allegrocache*
      (format t "Allegrocache DB already open; not opening...")
    (let* ((dbpath (reverse (subseq (reverse *acache-database-dir*) 1)))
           (fake (merge-pathnames "foo.txt" dbpath)))
      (format t "Opening Allegrocache db ~A~%" *acache-database-dir*)
      ;; When the database is opened in either mode, allegrocache sets
      ;; the value of db.ac:*allegrocache* to the open database 
      ;; connection object
      (progn
        ;; this should not be necessary, but...
        (ecase (os?)
          (:windows (ensure-directories-exist fake))
          (:unix nil))
        (db.ac:open-file-database 
         dbpath
         :if-exists :open
         :if-does-not-exist :create
         :verify user::*ofd-mode*
         )
        (unless db.ac:*allegrocache* 
          (error "Internal error.  Acache not opened!"))
        (format t "AllegroCacheDB ~A opened.~%" *acache-database-dir*)
        (setq *acache-running* t)
        ))))

        
    

