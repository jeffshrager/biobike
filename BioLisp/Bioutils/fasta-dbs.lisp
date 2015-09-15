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

;;;; FASTA databases.


;;; Create a FASTA database named DB-NAME from constituent files DB-CMPT-FILES

(defun create-fasta-db 
    (db-name db-cmpt-files
     &key 
     (if-exists :error)
     (key-function 'default-fasta-key-function)
     (verbose (or *fdbg* *fasta-verbose*)))

  #.(one-string-nl
     "Create a FASTA database out of a set of component files, DB-CMPT-FILES."
     "The new database file will be called DB-NAME."
     "See the FASTA API document for more details on all FASTA functions.")

  (let ((*fasta-verbose* verbose)
        (dbpath (pathname db-name)) 
        dbfile 
        (cmpt-paths nil))

    ;; See if a file already exists, and deal with it appropriately.
    ;; If the file specified has a file type, make sure it is the
    ;; right type; if no type, provide the type.

    (if (pathname-type dbpath)
	(unless (is-of-database-ft dbpath)
	  (error "The file type for a FASTA database must be ~A"
		 *fasta-database-ft*))
      (setq dbpath (pathname-of-database-ft dbpath)))
    ;; Cannot use TRUE-NAMESTRING here because file may not exist.
    (setq dbfile (namestring dbpath))
    (when (probe-file dbpath)
      (ecase if-exists
	(:error (error "The FASTA database file already exists: ~A" dbfile))
	((:supersede :overwrite) 
	 (when *fasta-verbose*
	   (format t "~&;; Overwriting existing FASTA database ~A~%" dbfile)))
	(:ask
	 (unless (yes-or-no-p 
		  "The FASTA database file ~A exists.  Overwrite?" dbfile)
	   (return-from create-fasta-db nil)))
  ; *** NEW
        (:no-op
           (RETURN-FROM create-fasta-db dbpath))
	   ))

    ;; Make sure all the component DB-FILES 
    ;; which are to be the constituents of our new FASTA DB exist.
    
    (unless (listp db-cmpt-files) 
      (setq db-cmpt-files (list db-cmpt-files)))

    (dolist (cmpt-file db-cmpt-files)
      (let ((cmpt-filepath (pathname cmpt-file)))
        (unless (probe-file cmpt-filepath)
          (error "File ~A does not exist!" cmpt-file))
	(push cmpt-filepath cmpt-paths)
	))
	
    (setq cmpt-paths (reverse cmpt-paths))

    ;; Okay, we have the path of the new DB file (DBPATH)
    ;; and all the constituent paths (CMPT-PATHS), and we know
    ;; all the constituents exist.
	
    (when *fasta-verbose*
      (format t "~&;; Creating new FASTA database file ~A with:~%" 
              (namestring dbfile))
      (dolist (c cmpt-paths)
	(format t ";;   Component file: ~A~%" (true-namestring c)))
      (terpri))

    (create-fasta-db-internal dbpath cmpt-paths key-function)

    ))

(defun create-fasta-db-internal (dbpath cmpt-paths key-function)

  ;; merge all the component dbs into a master db

  (let* ((master-hash-table (make-hash-table :test #'equal))
         (master-db-handle
          (make-instance 
           'fastadb :index master-hash-table :files nil :streams nil))
         (cmpt-db-handle (make-instance 'fastadb))
         (db-type nil))

    ;; Merge each component in turn

    (dolist (cmpt-path cmpt-paths)

      (cond
       ((is-of-database-ft cmpt-path) (setq db-type :dbfile))
       (t (setq db-type :fastafile))
       )

      ;; Create (if necessary) and load the compiled version of the component
      ;; database definition.

      (let ((cmptfile (true-namestring cmpt-path)) ipath cpath)
        (when *fasta-verbose* (format t "~&;; Merging file ~A~%" cmptfile))
        (ecase db-type
          (:dbfile 
           (setq cpath (pathname-of-compiled-database-ft cmpt-path))
           (ensure-compiled-db-file cmpt-path cmptfile cpath))
          (:fastafile
           (setq ipath (pathname-of-index-ft cmpt-path))
           (setq cpath (pathname-of-compiled-index-ft cmpt-path))
           (ensure-compiled-fasta-index-file
            cmpt-path cmptfile ipath cpath key-function
            )))
        (load-database-definition cpath db-type cmpt-db-handle))

      ;; Merge the component database definition against the master.

      (merge-fasta-dbs 
       master-db-handle cmpt-db-handle dbpath cmpt-path db-type
       ))
      
    ;; We merged them all.  Write the master as the new database definition.

    (write-fasta-db-definition dbpath master-db-handle)
 ; **** NEW
    dbpath      
    ))



(defun merge-fasta-dbs (master-db merge-db master-path merge-path ft)

  (let* ((master-ht (fastadb-index master-db))
         (master-files (coerce (fastadb-files master-db) 'list))
         (merge-ht (fastadb-index merge-db))
         (merge-files (coerce (fastadb-files merge-db) 'list))
         (n-master-files (length master-files))
         (nextpos n-master-files)
         (merge-filename (true-namestring merge-path))
         (duplicate-files-index nil)
         (nonduplicate-files-index nil)
         (nonduplicate-files nil)
         (new-hash-value nil)
         )

    (when *fdbg* (display-db master-db "Master"))
    (when *fdbg* (display-db merge-db "Merge"))

    ;; In the db definition to be merged, we have two sets of files,
    ;; those that are already components in the master db (duplicates)
    ;; and those that are not (nonduplicates).

    ;; The nonduplicates be will appended to the end of the master file list
    ;; (not at the beginning, because then the file index pointer of all
    ;; the entries in the master hash table would change!)

    ;; So we must compute a mapping for all nonduplicate files
    ;; from their original position (in the MERGE file list)
    ;; to their new position (in the MASTER file list)
    ;; This is NONDUPLICATE-FILES-INDEX.

    ;; Not allowing duplicated components would be infinitely easier!

    (do ((pos 0 (1+ pos)) (cmpt-list merge-files (cdr cmpt-list)))
        ((null cmpt-list))
      (let ((cmpt-file (first cmpt-list)))
        (if (member cmpt-file master-files :test #'string-equal)
          (progn
            (duplicate-file-note master-path merge-filename cmpt-file ft)
            (push pos duplicate-files-index))
          (progn
            (push (list pos nextpos) nonduplicate-files-index)
            (incf nextpos)
            (push cmpt-file nonduplicate-files)
            ))))
    (setq master-files (append master-files (reverse nonduplicate-files)))
    (setf (fastadb-files master-db) (coerce master-files 'vector))

    ;; Now put each entry from the MERGE-HT into the MASTER-HT,
    ;; making sure to adjust its FILEINDEX before inserting it
    ;; into MASTER-HT.  Ignore entries whose origin is a 
    ;; duplicate file, since the entry must already exist.

    (with-hash-table-iterator (next-entry merge-ht)
      (loop
       (multiple-value-bind (more? key value) (next-entry)
         (when (not more?) (return))
         (setq new-hash-value value)
         (let ((fileindex (fri-file-id new-hash-value)))
           ;; Ignore entries in duplicated files
           (unless (member fileindex duplicate-files-index)
             ;; Make sure entries in nonduplicated files don't
             ;; have same key as existing entries in MASTER-HT
             (let ((master-hash-value (gethash key master-ht)))
               (when master-hash-value
                 (duplicate-key-error 
                  key master-hash-value master-path 
                  merge-path master-files)))
             ;; Adjust file index of hash table value from MERGE-HT
             (let ((new-fileindex
                    (cadr (assoc fileindex nonduplicate-files-index))))
               (when (null new-fileindex)
                 (error "Internal error.  No new position found!!"))
               ;; Put adjusted value into the MASTER-HT
               (setf (fri-file-id new-hash-value) new-fileindex)
               (setf (gethash key master-ht) new-hash-value)
               ))))))

    (when *fdbg* (display-db master-db "Resulting master"))

    ))


(defun duplicate-key-error (key value dbpath cmpt-path master-files)
  (error 
   "The key ~S ~A~%  ~A~%~A~%  ~A~%~A ~A,~%  ~A~%~A"
   key
   "has already been stored in the database you are creating:"
   (namestring dbpath)
   "It has been defined already in included component file"
   (aref master-files (fri-file-id value))
   "The component file"
   (true-namestring cmpt-path)
   "now being included, also has this key."
   "Keys in a database must be unique!!"))


(defun duplicate-file-note (master-path merge-filename cmpt-file ft)
  (format t "~&~%;; Note: The FASTA ~Afile ~A~%" 
          (ecase ft (:dbfile "database ") (:ft "")) merge-filename)
  (format t ";;   being used as a component of the new FASTA database~%")
  (format t ";;   you are creating: ~A,~%" (namestring master-path))
  (ecase ft
    (:dbfile
     (format t ";;   has a component file ~A~%" cmpt-file)
     (format t ";;   which has already been included in your new database~%"))
    (:fastafile
     (format t ";;   has already been included in your new database~%")))
  (format t ";;   because it is or is part of one of the components~%")
  (format t ";;   that you listed earlier in the component list.~%")
  (terpri)
  )
     

(defun write-fasta-db-definition (dbpath db-handle)
  (let ((hash-table-to-write-out (fastadb-index db-handle))
	(component-file-vector (fastadb-files db-handle))
        (datum-list nil))
    (unless (plusp (length component-file-vector))
      (error "Internal error.  No files for FASTA DB definition."))
    (maphash 
     #'(lambda (key value) 
         (push (make-fasta-index-datum key value) datum-list))
     hash-table-to-write-out
     )
    (setq datum-list (nreverse datum-list))
    (when *fasta-verbose*
      (format t "~&;; Creating new FASTA database definition file: ~%")
      (format t ";;   ~A~%" (namestring dbpath)))
    (write-fasta-db-definition-internal
     dbpath component-file-vector datum-list :dbfile)
    (when *fasta-verbose*
      (format t ";; Creating compiled database definition file: ~%")
      (format t ";;   ~A~%" 
              (namestring (pathname-of-compiled-database-ft dbpath))))
    (maybe-compile-to-type-and-load 
     dbpath *fasta-compiled-database-ft* :compile? t :load? nil)
    ))

