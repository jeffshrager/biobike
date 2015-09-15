;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

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

;;; Author:  JP Massar.  Woo hoo.


;;; How to run queries:
;;; For example: 
;;;  (bio::with-organisms-db (db) 
;;;    (bio::esql 
;;;      "select id from genes where organism = 'anabaena7120' limit 100"))

;;; Or just (bio::dosql <query>)

;;; Or open the database using OODB and then use ESQL commands.


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *dbaccess-api-symbols*
    '(
     *db-verbose*
     with-organisms-db
     oodb
     dosql
     mysql-escape
     ))

  (export *dbaccess-api-symbols* (find-package :biolisp)))

(defparameter *db-verbose* t "Show database queries as they execute")
(defparameter *db-execute* t)

;; So we don't need the dbi.mysql package to develop the code.

#+:allegro-cl-enterprise
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :dbi.mysql) (use-package :dbi.mysql)))

(defun execute-db-command (name args)
  (when *db-execute* (apply (intern (string name) :dbi.mysql) args)))

(defun mysql-escape (&rest s) 
  "Return a string suitable for handing off to MYSQL"
  (execute-db-command 'mysql-escape-sequence s))

(defvar *with-organisms-db-level* -1)
(defvar *organisms-db-connection* nil)

(defmacro with-organisms-db 
          ((dbconnection &key (verbose? '*db-verbose*))
           &body body)
  #.(one-string-nl
     "Make sure an open database connection exists for the dynamic scope of "
     "this form.  The symbol DBCONNECTION is bound to that open connection. "
     "The database connection is closed on exit from this form iff t was not "
     "already open when this form was entered.")
  `(let* ((*db-verbose* ,verbose?)
          (,dbconnection (or *organisms-db-connection* (bioconnect)))
          (*organisms-db-connection* ,dbconnection)
          (*with-organisms-db-level* (1+ *with-organisms-db-level*)))
     (unwind-protect
         (progn ,@body)
       (when (zerop *with-organisms-db-level*)
         (edisconnect :db ,dbconnection)
         ))))

(defmacro dosql (string &rest args)
  #.(one-string-nl
     "Execute an SQL command. If ARGS exist STRING is treated as a format "
     "string with ARGS as its format args.  Otherwise STRING is used as is. "
     "This is intended as an interactive tool, not for use within inner "
     "loops of programs as it is rather inefficient.")
  `(with-organisms-db (db) 
     (esql ,(if (null args) string `(format nil ,string ,@args)))))

(defun esql (&rest args) 
  (when *db-verbose*
    (format 
     t 
     "~&;; ~A SQL query: ~A~%" 
     (if *db-execute* "Executing" "Not executing")
     (first args)))
  (execute-db-command 'sql args))

(defun econnect (&rest args) (execute-db-command 'connect args))

(defun bioconnect (&key (db-name cl-user:*default-bio-database-name*))
  (apply 'econnect :database db-name cl-user::*biosql-connect-args*))

(defun edisconnect (&rest args) (execute-db-command 'disconnect args))


;;; Open Organisms DataBase

(defun oodb () "Open the Organisms DataBase" (bioconnect))


