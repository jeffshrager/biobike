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

;;; Author:  JP Massar.

;;; The class containing in-memory information about a FASTA database.
;;; The STREAMS slot is not used at this time.  It would be used to
;;; cache open connections to FASTA source files, so that the system
;;; would not have to call OPEN every time it wanted to retrieve a record.

(defclass fastadb ()
  ((files :accessor fastadb-files :initarg :files :initform nil)
   (index :accessor fastadb-index :initarg :index :initform nil)
   (streams :accessor fastadb-streams :initarg :streams :initform nil)
   (type :accessor fastadb-type :initarg :type :initform nil)
   ))

(defun display-db (db-handle &key (title "") (count :all))
  (format t "~&~%Pprint for Fasta database ~S~%" title)
  (let* ((ht (fastadb-index db-handle))
         (files (coerce (fastadb-files db-handle) 'list))
         (type (fastadb-type db-handle))
         (line-limit (if (eq count :all) (hash-table-count ht) count)))
    (format t "  Component files:~%")
    (do ((f files (cdr f)) (j 0 (1+ j))) ((null f))
      (format t "    [~2D] ~A~%" j (car f)))
    (format t "  Hash table contents (<key> || <value>):~%")
    (let ((j 0))
      (block exit
        (maphash 
         #'(lambda (key value) 
             (format t "    ~A || ~A~%" key value)
             (if (>= (incf j) line-limit) (return-from exit nil)))
         ht
         )))
    (format t "  Database type: ~S~%" type)
    (terpri)))


;;; Loads a database definition file, which can be a source
;;; FASTA database or FASTA index file, or its compiled version.
;;; It does some sanity checking to make sure that what we're loading
;;; does indeed seem to be a FASTA database definition.

;;; If DB-HANDLE is provided, it is assumed to be a FASTADB instance,
;;; and its slots are filled in appropriately.  All the code in this
;;; implementation calls LOAD-DATABASE-DEFINITION with a DB-HANDLE.

(defun load-database-definition (path db-type &optional (db-handle nil))
  (setq *fasta-files* nil *fasta-index* nil)
  (load-silently path)
  ;; Verify that the file we loaded defined the database information
  ;; we need!
  (labels ((internal-error (file-description filename file-variable)
	     (error "Internal error!! Compiled ~A file ~A does not ~A ~A"
		    file-description
                    filename
		    "contain a valid definition for"
		    file-variable))
	   (index-error? (file-description)
	     (unless *fasta-index*
	       (internal-error 
		file-description (true-namestring path) '*fasta-index*)))
           (verify (description)
             (index-error? description)
             (unless (and *fasta-files* (vectorp *fasta-files*))
	       (internal-error 
                "database" (true-namestring path) '*fasta-files*))))
    (ecase db-type
      (:dbfile (verify "database"))
      (:fastafile (verify "FASTA index"))))
  (when db-handle
    (setf (fastadb-index db-handle) *fasta-index*)
    (setf (fastadb-files db-handle) *fasta-files*)
    (setf (fastadb-streams db-handle)
      (make-sequence 'vector (length *fasta-files*) :initial-element nil))
    (setf (fastadb-type db-handle) db-type))
  nil
  )
	     

(defmacro with-fasta-db 
          ((db-handle-var dbfile &rest open-fasta-db-keys)  &body body)
  #.(one-string-nl
     "Open a FASTA database and create a handle (bound to DB-HANDLE-VAR) "
     "for it for the duration of BODY.  See the FASTA API document for "
     "more details on all FASTA functions.")
  `(let ((,db-handle-var (open-fasta-db ,dbfile ,@open-fasta-db-keys)))
     (unwind-protect
         (progn ,@body)
       (close-fasta-db ,db-handle-var)
       )))


(defun open-fasta-db 
    (dbfile 
     &key 
     (key-function #'default-fasta-key-function)
     (verbose (or *fdbg* *fasta-verbose*)))

  #.(one-string-nl
     "Open a FASTA database, returning a handle to that DB. "
     "See the FASTA API document for more details on all FASTA functions.")

  ;; If DBFILE does not exist, TRUE-NAMESTRING will blow up here.
  ;; (Which is fine).

  (let* ((*fasta-verbose* verbose)
         (dbpath (pathname dbfile))
	 (dbfile (true-namestring dbpath))
	 (db-handle (make-instance 'fastadb)))

    (cond

     ;; We are opening a FASTA DATABASE file.

     ((is-of-database-ft dbpath)
      (let ((cpath (pathname-of-compiled-database-ft dbpath)))
        (ensure-compiled-db-file dbpath dbfile cpath)
        (when *fasta-verbose*
          (format t "~&;; Loading compiled definition file ~A~%"
                  (true-namestring cpath))
          (format t ";;   for FASTA database file ~A~%" dbfile))
        (load-database-definition cpath :dbfile db-handle)
        ))
	
     ;; We are opening a FASTA file.
     ;; Create the index file and/or its compiled version, as necessary.

     (t
      (let ((ipath (pathname-of-index-ft dbpath))
            (cpath (pathname-of-compiled-index-ft dbpath)))
        (ensure-compiled-fasta-index-file 
         dbpath dbfile ipath cpath key-function)
        (when *fasta-verbose*
          (format t "~&;; Loading compiled index file ~A~%" 
                  (true-namestring cpath))
          (format t ";;   for FASTA file ~A~%" dbfile))
        (load-database-definition cpath :fastafile db-handle)
        ))
     
     )

    db-handle

    ))

(defun close-fasta-db (db-handle)
  #.(one-string-nl
     "Close the FASTA database associated with DB-HANDLE."
     "See the FASTA API document for more details on all FASTA functions.")
  (map nil #'(lambda (s) (and s (close s)))  (fastadb-streams db-handle)))


(defmacro with-fasta-db-keys ((key db-handle) &body body)
  #.(one-string-nl
     "Iterate through all the records in the FASTA database associated "
     "with DB-HANDLE, executing BODY.  KEY is bound to successive keys "
     "of the database each time BODY is executed. "
     "See the FASTA API document for more details on all FASTA functions.")
  (let ((ht-symbol (gensym "HT-"))
        (next-key-symbol (gensym "NEXT-KEY-"))
        (next-value-symbol (gensym "NEXT-VALUE-"))
        )
    `(let ((,ht-symbol (fastadb-index ,db-handle)))
       (maphash
        #'(lambda (,next-key-symbol ,next-value-symbol) 
            (declare (ignore ,next-value-symbol))
            (let ((,key ,next-key-symbol))
              ,@body
              ))
        ,ht-symbol
        ))))
