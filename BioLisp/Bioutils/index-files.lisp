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

;;; Take a FASTA INDEX data structure and write it out to an index
;;; file such that when we call LOAD on that index file the fasta
;;; index will be consed up in core (as a hash table).

;;; This is how a FASTA INDEX or FASTA DATABASE DEFINITION file should look
;;; In a FASTA INDEX file all FILEINDEX's are 0 and instead of :dbfile
;;; we put :fastafile in the call to MAKE-FASTA-DATABASE-HASH-TABLE.
	     
#|

(in-package :bioutils)

(setq *fasta-files*
    #(<file0> <file1> ...))

(setq *fasta-data*
      '(("<key1>" . <fri1>)
        ("<key2>" . <fri2>)
        ...
        ))

;; Where an FRI is a vector of data about a record.

(setq *fasta-index*
    (make-fasta-database-hash-table *fasta-files* *fasta-data* :dbfile))

|#	 

(defmethod fasta-index-to-file 
    (index (as (eql :hash-table))
     &key 
     (table-bound-to '*fasta-index*)
     (file-bound-to '*fasta-files*)
     (data-bound-to '*fasta-data*)
     (package :cl-user)
     (extension *fasta-index-ft*)
     (output-file nil)
     (compile? t)
     (load? t)
     )
  (let* ((source-file (true-namestring (fasta-index-source-file index)))
	 (data (fasta-index-data index))
	 (index-path
          (or (and output-file (pathname output-file))
              (pathname-of-ft (pathname source-file) extension))))
    ;; Set the file id of each record.
    (dolist (fasta-index-datum data) 
      (let ((fri (fasta-index-datum-recordinfo fasta-index-datum)))
        (setf (fri-file-id fri) 0)))
    ;; Create the index file
    (when *fasta-verbose*
      (format t "~&;; Creating index file ~A~%" (namestring index-path)))
    (write-fasta-db-definition-internal
     index-path (vector source-file) 
     data :fastafile
     :package package 
     :filevar file-bound-to
     :datavar data-bound-to 
     :hashvar table-bound-to)
    ;; Compile/load it as appropriate.
    (maybe-compile-to-type-and-load
     index-path *fasta-compiled-index-ft* :compile? compile? :load? load?)
    ))

      
(defun write-fasta-db-definition-internal
    (output-path file-vector data db-type
     &key 
     (package :bioutils)
     (filevar 'cl-user::*fasta-files*)
     (datavar 'cl-user::*fasta-data*)
     (hashvar 'cl-user::*fasta-index*)
     )
  (with-open-file (f output-path :direction :output :if-exists :supersede)
    (flet ((pprint-form (form) (when form (pprint form f) (terpri f))))
      (with-standard-io-syntax
	(pprint-form 
	 `(in-package
	   ,(intern (package-name (find-package package)) :keyword)))
	(pprint-form `(setq ,filevar ,file-vector))
	(pprint-form `(setq ,datavar ',data))
	(pprint-form
	 `(setq ,hashvar 
	      (make-fasta-database-hash-table ,datavar ,db-type)))
	)))
  output-path
  )


;;; And this function creates the hash table itself from the data.
;;; It gets called when the DATABASE definition is LOADED.

(defun make-fasta-database-hash-table (data db-type)
  (declare (ignore db-type))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (fasta-index-datum data)
      (let ((key (fasta-index-datum-key fasta-index-datum))
            (data (fasta-index-datum-recordinfo fasta-index-datum)))
        (setf (gethash key ht) data)
        ))
    ht
   ))

