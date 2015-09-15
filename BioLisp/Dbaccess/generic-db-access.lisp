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

;;; Author:  JP Massar.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;  GENERIC INTERFACE TO DATABASE CONNECTIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; We envision a generic interface to SQL databases which gets specified
;;; along two axes:  
;;;   -- different Lisp implementations' access methodologies to particular
;;;      databases, such as MYSQL
;;;   -- different SQL databases, such as ORACLE vs MYSQL vs ?

;;; (Currently we implement only Allegro's methodology for MYSQL)

;;; This interface assumes multiple users in a threaded environment,
;;; each of whom is allowed to have their own individual connections
;;; to the same or different databases.

(defparameter *my-current-db* nil)

;;; We assume there is a variable *my-current-db* in each user's
;;; package, defined using the SAVED-VARIABLES mechanism from the UTILS
;;; package (or a similar mechanism).  We define a default variable of the 
;;; same name in the GENERICDB package above.
;;; This variable should initially be NIL in all packages, or set to
;;; a DB-SERVER-CONNECTION object (see below).

(defun my-current-db-symbol ()
  #.(one-string-nl
     "Returns the symbol CL-USER::*MY-CURRENT-DB* unless the weblistener"
     "package exists and the value of the symbol WB:*USERNAME* is the name"
     "of an existing package; if so the *MY-CURRENT-DB* symbol in that"
     "package is returned.  If the symbol doesn't exist it's created and"
     "if it's unbound it is set to NIL")
  (block exit
    (vwhen (wbpkg (find-package :wb))
      (vwhen (package-name 
              (forward-package-funcall :wb :user-session-package-name))
        (vwhen (user-pkg (and (symbolp package-name) 
                              (find-package package-name)))
          (let ((symbol
                 (symbol-of-package 
                  '*my-current-db* user-pkg :if-does-not-exist :intern)))
            (when (not (boundp symbol)) 
              (eval `(common-lisp:defvar ,symbol common-lisp:nil)))
            (return-from exit symbol)
            ))))
    '*my-current-db*
    ))

(defparameter *generic-db-verbose* t 
  "Show database queries as they execute")
(defparameter *generic-db-execute* 
  #+:allegro t
  #-:allegro nil
  "If NIL, don't actually try to execute queries.  For development/debugging.")

;;; If the weblistener is loaded we assume there is configuration variable
;;; cl-user:*default-db-connection-methodology*
;;; which is accessed.  If the GENERICDB code is run standalone the variable
;;; genericdb::*default-db-connection-methodology* is accessed instead.

;;; This specifies which DB connection methodology is the default.
;;; A methodology is simply a keyword, the only legal value now being :MYSQL.
;;; (If a particular database has different methodologies depending on the
;;; Lisp implementation that the different implementations used to define
;;; this interface should be separated using #+ and friends)

;;; *****>  IF YOU ARE USING THIS PACKAGE STANDALONE DEFINE THIS  <*****
;;; *****>  VARIABLE APPROPRIATELY, OTHERWISE WHEN USING THE     <*****
;;; *****>  WEBLISTENER DEFINE IT IN THE CONFIGURATION FILE       <*****

(defvar *default-db-connection-methodology* :mysql)

(defun default-db-connection-methodology ()
  (if (find-package :wb)
      (symbol-value-in-package '*default-db-connection-methodology* :wb)
    *default-db-connection-methodology*
    ))


;;; Each connection methodology must implement a class
;;; whose supertype is DB-SERVER-CONNECTION.  E.g., MYSQL-CONNECTION.

(defclass db-server-connection () 
  ((connect-level :accessor dbsc-connect-level :initform 0)))

;;; Each methodology must also implement methods for the generic functions:

(defgeneric make-default-db-server-connection (type))

(defun make-default-dbsc (type) (make-default-db-server-connection type))

(defmethod make-default-db-server-connection ((type (eql t)))
  (make-default-db-server-connection (default-db-connection-methodology)))

(defgeneric make-db-server-connection (type &rest args &key &allow-other-keys))

(defun make-dbsc (type &rest args)
  (apply 'make-db-server-connection type args))

(defgeneric open-db-server-connection (obj))

(defgeneric close-db-server-connection (obj))

;;; The generic functions XSQL and XXSQL are used to send SQL queries to
;;; a database through a connection.  If the first argument to XSQL or XXSQL is
;;; an DB-SERVER-CONNECTION object, the appropriate method based on the
;;; subtype is used.  If the first argument to XSQL or XXSQL is a string, 
;;; the string (or the string and the other FORMAT-ARGS) are used as the
;;; text of the SQL command, while the default DB-SERVER-CONNECTION object
;;; (as defined above) is obtained or created, and the appropriate method
;;; based on this default DB-SERVER-CONNECTION object is then called.

;;; XSQL and XXSQL wraps their execution in a WITH-OPEN-DB-SERVER-CONNECTION 
;;; so that a newly created connection is never left open (but one that is
;;; already open will remain open, as per WODSC's semantics).

;;; The difference between XSQL and XXSQL is that XSQL treats its &rest
;;; argument, FORMAT-ARGS, as a format string and arguments to be used
;;; to create a string to feed to SQL as the text of the command.
;;; XXSQL treats its &rest argument, QUERY-ARGS, such that the first &rest arg
;;; is considered the text of the SQL command, and the other &rest arguments
;;; are assumed to be keyword-value pairs that get passed on to the underlying
;;; Lisp function that implements the execution of an SQL command.


;;; These two have default methods defined below.
(defgeneric xsql (connection-specifier &rest format-args))
(defgeneric xxsql (connection-specifier &rest query-args))

(defgeneric open-db-server-connection? (db-server-obj))

;;; These last two are optional because they have default methods.

(defgeneric increment-db-server-connection-level (db-server-obj))

(defgeneric decrement-db-server-connection-level (db-server-obj))

(defmethod increment-db-server-connection-level ((obj db-server-connection))
  (incf (dbsc-connect-level obj)))

(defmethod decrement-db-server-connection-level ((obj db-server-connection))
  (decf (dbsc-connect-level obj)))


;;; For many people, the only thing they will ever need to do
;;; is to use XSQL or the macro

;;; WITH-OPEN-DB-SERVER-CONNECTION

;;; It's default usage is:

;;; (with-open-db-server-connection (t) &body body-forms)

;;; If *MY-CURRENT-DB* has the value NIL:

;;; This opens a connection using *DEFAULT-DB-CONNECTION-METHODOLOGY*
;;; and the methods MAKE-DEFAULT-DB-SERVER-CONNECTION and
;;; OPEN-DB-SERVER-CONNECTION.  It binds *MY-CURRENT-DB* in the user's
;;; package (or in the CL-USER package, if no user is active) to the
;;; value returned by MAKE-DEFAULT-DB-SERVER-CONNECTION.
;;; The connection is open for the duration of BODY-FORMS and is closed
;;; upon exit (using UNWIND-PROTECT to guarentee the connection is closed
;;; via a call to the CLOSE-DB-SERVER-CONNECTION method).

;;; If *MY-CURRENT-DB* is an unopened DB-SERVER-CONNECTION object then it
;;; is used and opened (by calling the OPEN-DB-SERVER-CONNECTION method on
;;; the object).

;;; If *MY-CURRENT-DB* is an already open DB-SERVER-CONNECTION
;;; it is not opened again; it's CONNECT-LEVEL is simply incremented.
;;; (This allows WODSC forms to be nested without a problem and without
;;; significant overhead).

;;; Other possible syntax for the arguments is 

;;; ... (symbol) &body body-forms)

;;; In which case SYMBOL is bound to a DB-SERVER-CONNECTION object,
;;; which is always newly created.

;;; ... ((t expression)) &body body-forms)

;;; In which case EXPRESSION is evaluated and must produce a 
;;; DB-SERVER-CONNECTION object to which *MY-CURRENT-DB* is bound.
;;; If the object so produced is not opened it is opened in the standard
;;; way, otherwise it's CONNECT-LEVEL is incremented.

;;; This would be the usual mechanism for creating a connection to a
;;; non-default database.

;;; ... ((symbol expression)) &body body-forms

;;; And lastly EXPRESSION is evaluated and must produce a DB-SERVER-CONNECTION
;;; object to which SYMBOL is bound.

;;; This would be the mechanism used to create a connection to a non-default
;;; database while maintaining an open connection to another database.

;;; Finally, keyword arguments specifying arguments to call the routine
;;; which creates the DB-SERVER-CONNECTION object are allowed.

;;; The complete sytax is

;;; (with-open-db-server-connection 
;;;      (connection-binding &key make-instance-args)
;;;   &body body)

;;; WITH-OPEN-DB-SERVER-CONNECTION is implemented using the function

;;; CREATE-DB-SERVER-CONNECTION &rest args &key (type :mysql) &allow-other-keys
;;;
;;; This function calls MAKE-INSTANCE with the appropriate 
;;; arguments for TYPE.

;;; This function may be called by the user to create non-dynamically scoped
;;; DB-SERVER-CONNECTION objects.

;;; The generic function

;;; OPEN-DB-SERVER-CONNECTION ((db-server-object <subtype>))
;;;
;;; actually opens a connection to a database server.  The user may call
;;; OPEN-DB-SERVER-CONNECTION on the object returned by
;;; CREATE-DB-SERVER-CONNECTION.  If successful two values are returned,
;;; the object passed in and either :ALREADY-OPEN or :OPENED.  If not
;;; successful an error is signalled.

;;; The generic function

;;; CLOSE-DB-SERVER-CONNECTION ((db-server-object <subtype))

;;; is used to close the connection.  T is returned or an error is signalled.



(defun maybe-create-open-and-increment-connection 
       (conn create-if-null? &rest creation-args)
  ;; If it's not a DB-SERVER-CONNECTION object create one.
  (when (and create-if-null? (null conn))
    (setq 
     conn 
     (apply 
      'make-db-server-connection
      (default-db-connection-methodology)
      creation-args
      )))
  (unless (typep conn 'db-server-connection)
    (error 
     "Ruh roh.  Connection argument, ~A, not a DB-SERVER-CONNECTION" conn))
  ;; If it's not open, open it.
  (when (not (open-db-server-connection? conn))
    (open-db-server-connection conn))
  ;; Increment the WITH-OPEN usage count
  (increment-db-server-connection-level conn)
  conn
  )

(defmacro do-dbsc-body (conn &body body)
  (let ((conn-value-symbol (gensym "CONN-VALUE-")))
    `(let ((,conn-value-symbol ,conn))
       (unwind-protect 
           (progn ,@body)
         (when (zerop (decrement-db-server-connection-level ,conn-value-symbol))
           (close-db-server-connection ,conn-value-symbol)
           )))))


(defmacro with-open-db-server-connection 
          ((connection-binding &rest create-db-server-args)
           &body body 
           &aux 
           conn-symbol
           conn-symbol-t?
           conn-expression
           expression-provided?
           (connect-var (gensym "CONN-SYMBOL-"))
           )

  ;; Parse CONNECTION-BINDING

  (cond
   ((symbolp connection-binding)
    (setq conn-symbol connection-binding)
    (setq conn-expression nil)
    (setq expression-provided? nil))
   ((listp connection-binding)
    (unless (= 2 (length connection-binding))
      (error "Invalid syntax to WITH-OPEN-DB-SERVER-CONNECTION."))
    (setq conn-symbol (first connection-binding))
    (setq conn-expression (second connection-binding))
    (setq expression-provided? t)
    ))

  (setq conn-symbol-t? (eq conn-symbol t))
  (unless (or conn-symbol-t? (not (constantp conn-symbol)))
    (error "Invalid syntax to WITH-OPEN-DB-SERVER-CONNECTION."))

  (cond
    (conn-symbol-t?
     `(let* ((,connect-var (my-current-db-symbol)))
        ;; This binds *MY-CURRENT-DB*
        (progv (list ,connect-var)
            (list (maybe-create-open-and-increment-connection
                   (symbol-value ,connect-var) 
                   ,(not expression-provided?)
                   ,@create-db-server-args
                   ))
          (do-dbsc-body (symbol-value ,connect-var) ,@body)
          )))
    (t
     `(let ((,conn-symbol ,conn-expression))
        (maybe-create-open-and-increment-connection 
         ,conn-symbol nil ,@create-db-server-args)
        (do-dbsc-body ,conn-symbol ,@body)
        ))))

(defmacro with-open-dbsc ((connection-binding &rest create-args) &body body)
  `(with-open-db-server-connection (,connection-binding ,@create-args)
     ,@body
     ))
   
(defmethod xsql ((connection-specifier string) &rest format-args)
  ;; Set up a standard connection and then dispatch to the XSQL
  ;; routine which handles that kind of connection.
  (with-open-db-server-connection (t)
    (let ((format-string connection-specifier))
      (apply 
       'xsql (symbol-value (my-current-db-symbol)) format-string format-args)
      )))
   
(defmethod xxsql ((connection-specifier string) &rest query-args)
  ;; Set up a standard connection and then dispatch to the XSQL
  ;; routine which handles that kind of connection.
  (with-open-db-server-connection (t)
    (let ((query-string connection-specifier))
      (apply 
       'xxsql (symbol-value (my-current-db-symbol)) query-string query-args)
      )))

