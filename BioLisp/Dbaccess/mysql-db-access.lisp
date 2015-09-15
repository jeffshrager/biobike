;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: genericdb; -*-

(in-package :genericdb)

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

;;; Author:  JP Massar.  Foo.  

;;; Implementation of DB-SERVER-CONNECTION for MYSQL

#+:allegro-cl-enterprise
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :dbi.mysql) (use-package :dbi.mysql)))

;; Hack to not get warning about these variables being defined in two
;; different files when the Weblistener system is loaded.
(defmacro special-export (symbol value)
  `(progn
     (declaim (special ,symbol))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',symbol :cl-user))
     (unless (boundp ',symbol) (setq ,symbol ,value))
     ))

(special-export cl-user::*default-mysql-local-file-arg* 
                "/var/lib/mysql/mysql.sock")
(special-export cl-user::*default-mysql-remote-machine* "nostoc.stanford.edu")
(special-export cl-user::*default-mysql-database-name* "cyanobacteria")
(special-export cl-user::*default-mysql-remote-user-name* "root")
(special-export cl-user::*default-mysql-remote-password* nil)
(special-export cl-user::*default-mysql-local-or-remote-default* :local)


(defclass mysql-connection (db-server-connection)
  (
   (host :initarg :host :accessor mysql-host :initform :not-provided)
   (port :initarg :port :accessor mysql-port :initform :not-provided)
   (file :initarg :file :accessor mysql-file :initform :not-provided)
   (dbname :initarg :dbname :accessor mysql-dbname :initform :not-provided)
   (user :initarg :cl-user :accessor mysql-user :initform :not-provided)
   (password 
    :initarg :password 
    :accessor mysql-password
    :initform :not-provided
    )
   ;; This is computed from the above values
   (connect-args :accessor mysql-connect-args :initform nil)
   ;; This is set once the connection is opened and set back to NIL when closed
   (internal-connection :accessor mysql-internal-connection :initform nil)
   (connect-level :accessor mysql-connect-level :initform 0)
   ))

(defmethod initialize-instance :after
           ((obj mysql-connection) 
            &rest initargs
            &key 
            (host nil hostp)
            (port nil portp)
            (file nil filep)
            (dbname nil dbnamep)
            (user nil userp)
            (password nil passwordp)
            &allow-other-keys
            )
  (when filep
    (when host 
      (error "Do not specify both HOST and FILE for MYSQL connection"))
    (when port
      (error "Do not specify both PORT and FILE for MYSQL connection")))
  
  (setf (mysql-connect-args obj)
        `(,@(and hostp `(:host ,host))
          ,@(and portp `(:port ,port))
          ,@(and filep `(:file ,file))
          ,@(and dbnamep `(:database ,dbname))
          ,@(and userp `(:user ,user))
          ,@(and passwordp password `(:password ,password))
          ))

  obj

  )
  
(defmethod open-db-server-connection? ((obj mysql-connection))
  (not (null (mysql-internal-connection obj))))


(defmethod make-default-db-server-connection ((type (eql :mysql)))
  (ecase cl-user:*default-mysql-local-or-remote-default*
    (:os
     (ecase (cl-user::os?)
       ;; Assume a local connection via :file mechanism
       (:unix (make-default-local-db-server-connection))
       (:windows (make-default-remote-db-server-connection))
       ))
    (:local (make-default-local-db-server-connection))
    (:remote (make-default-remote-db-server-connection))
    ))

(defun make-default-local-db-server-connection ()
  (make-instance 
   'mysql-connection
   :file cl-user:*default-mysql-local-file-arg*
   :user cl-user:*default-mysql-local-user-name*
   :password cl-user:*default-mysql-local-password*
   :dbname cl-user:*default-mysql-database-name*
   ))  

(defun make-default-remote-db-server-connection ()
  (make-instance
   'mysql-connection
   :host cl-user:*default-mysql-remote-machine*
   :dbname cl-user:*default-mysql-database-name*
   :user cl-user:*default-mysql-remote-user-name*
   :password cl-user:*default-mysql-remote-password*
   ))


(defmethod make-db-server-connection 
           ((type (eql :mysql)) &rest args &key &allow-other-keys)
  (if (null args)
      (make-default-db-server-connection type)
    (apply 'make-instance 'mysql-connection args)
    ))

(defmethod open-db-server-connection ((obj mysql-connection))
  (if (mysql-internal-connection obj)
      (values obj :already-open))
    (let ((conn (apply 'exec-mysql-command :connect (mysql-connect-args obj))))
      (unless (or (null *generic-db-execute*) conn)
        (error "MYSQL CONNECT attempt returned NIL !!"))
      (setf (mysql-internal-connection obj) (or conn t))
      (values obj :opened)
      ))

(defmethod close-db-server-connection ((obj mysql-connection))
  (let ((ic (mysql-internal-connection obj)))
    (setf (mysql-internal-connection obj) nil)
    (if (null ic)
        (progn
          (warn "Connection being closed was not open: ~A" obj)
          nil)
      (progn
        (exec-mysql-command :disconnect :db ic)
        t))))


(defmethod xsql ((obj mysql-connection) &rest format-args)
  (with-open-db-server-connection ((x obj))
    (let ((command 
           (if (cdr format-args) 
               (apply 'format nil (first format-args) (rest format-args))
             (first format-args))))
      (when *generic-db-verbose*
        (cformatt 
         "~A SQL query: ~S"
         (if *generic-db-execute* "Executing" "(Not executing)")
         command
         ))
      (exec-mysql-command :sql command :db (mysql-internal-connection x))
      )))
       
(defmethod xxsql ((obj mysql-connection) &rest query-args)
  (with-open-db-server-connection ((x obj))
    (when *generic-db-verbose*
      (cformatt 
       "~A SQL query: ~A"
       (if *generic-db-execute* "Executing" "(Not executing)")
       (first query-args)
       ))
      (apply
       'exec-mysql-command 
       :sql 
       (first query-args)
       :db (mysql-internal-connection x)
       (rest query-args)
       )))


(defun exec-mysql-command (name &rest args &aux (k 0) (limit 3))
  (prog ()
   again
    (when *generic-db-verbose*
      (cformatt "~A SQL command ~A, with arguments~%;; ~S"
		(if *generic-db-execute* "Executing" "(Not executing)")
		name args
		))
    (return
      (handler-case 
	  (when *generic-db-execute*
	    (apply (symbol-of-package name :dbi.mysql) args)
	    )
	(error (c)
	  (cformatt "In exec-mysql-command: DBI.MYSQL error.")
	  (cformatt "Actual error is ~a" c)
	  (incf k)
	  (cond ((= k limit) (error c))
		(t (go again))))
	))))

#+old
(defun exec-mysql-command (name &rest args)
  (when *generic-db-verbose*
    (cformatt "~A SQL command ~A, with arguments~%;; ~S"
              (if *generic-db-execute* "Executing" "(Not executing)")
              name args
              ))
  (when *generic-db-execute*
    (apply (symbol-of-package name :dbi.mysql) args)
    ))



(defun test ()
  (setq *my-current-db* nil)
  (let ((*generic-db-verbose* nil))
    (cformatt "Standard WODBSC form, simple query.")
    (with-open-dbsc (t) (xsql "show ~A" "tables"))
    (cformatt "Standard WODBSC form, XXSQL query.")
    (with-open-dbsc (t) (xxsql "show tables" :names nil))
    (cformatt "Nested WODBSC forms, simple query.")
    (with-open-dbsc (t)
      (cformatt "Outer")
      (with-open-dbsc (t)
        (cformatt "Inner")
        (with-open-dbsc (t) (xsql "select name from new_organisms limit 1"))
        ))
    (cformatt "*MY-CURRENT-DB* should be nil, is: ~A" *my-current-db*)
    (cformatt "Test created connection")
    (let ((dbsc (make-default-db-server-connection t)))
      ;; Test a created, non-opened one.
      (with-open-dbsc ((x dbsc))
        (xsql x "select name from new_organisms limit 2")
        ;; Test one that is already open.
        (with-open-dbsc ((y x))
          (xsql y "select name from new_organisms limit 3")
          )))
    (cformatt "*MY-CURRENT-DB* should be nil, is: ~A" *my-current-db*)
    ))
