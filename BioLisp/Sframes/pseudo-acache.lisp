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


;;; Define a skeleton package in lieu of Allegrocache
;;; so that we don't have to put #+:acache all over our code.
;;; Theoretically if an attempt were made to use anything in the db.ac package
;;; either a function not defined or an undefined method error should be 
;;; signaled.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not *acache-loaded*)
    (when (not (find-package :db.ac))
      (make-package 
       :db.allegrocache
       :nicknames '(:db.ac)
       :use '(:common-lisp :aframes))
      )))

(in-package :db.ac)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *acache-interface-functions*
    '(
      ac-map
      close-database
      delete-instance
      map-map
      map-value
      open-file-database
      open-map
      open-network-database
      remove-from-map
      start-server
      ))
  (when (not cl-user::*acache-loaded*)
    (export `(*allegrocache* ,@*acache-interface-functions*))
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not cl-user::*acache-loaded*)
    (defvar *allegrocache* nil)
    (eval '(defclass ac-map () ()))
    (defsetf map-value .inv-map-value) ;; set-map-value)
    (eval `(user::not-implemented 
            .inv-map-value 
            ;; set-map-value
            ,@*acache-interface-functions*))
    (defun delete-instance (x) (declare (ignore x)) nil)
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :db.ac :aframes))

(in-package :user)

(defun purge-cache-database ()
  (when db.ac:*allegrocache*
    (utils:forward-funcall 'db.ac:close-database db.ac:*allegrocache*))
  (handler-case 
      (utils:forward-funcall 
       'db.ac:open-file-database *acache-database-dir* :if-exists :supersede)
    (error 
     () 
     (format t "Database ~A does not exist.~%" *acache-database-dir*)
     nil)))
           
#+test
(defun load-test-db (&optional (supersede? :open)) 
  (utils:forward-funcall 
   'db.ac:open-file-database  
   "C:/lispcode/test-db"
   :if-does-not-exist :create
   :if-exists supersede?)
  )

(defun close-db ()
  (utils:forward-funcall 'db.ac:close-database db.ac:*allegrocache*))



