;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Author: JP Massar.

(in-package :bioutils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tests

(defun test-file-template ()
  (cl-user:translate-simple-lp "Biol:Testing;"))

(defun fasta-test-file (&optional (name "test.fsta"))
  (merge-pathnames name (test-file-template)))

(defparameter *fasta-other-extension* (fasta-test-file "test.fasta"))
(defparameter *fasta-no-extension* (fasta-test-file "test"))

;; Returns NIL if file does not exist or could not be deleted.
;; Otherwise T.  DELETE-FILE is always supposed to return T or error out,
;; but Lispworks returns NIL if file does not exist.
(defun delete-no-error (x)
  (when (probe-file x) 
    (handler-case (delete-file x)
      (error () (format t "~&;; ***> Could not delete existing file ~A~%" x)
             nil))))
                        
(defun write-test-strings-to-file (file strings)
  (with-open-file (p file :direction :output :if-exists :supersede)
    (loop for s in strings do (format p "~A~%" s))))

(defun write-fasta-file-and-test-parse
       (file-strings &optional (testfile (fasta-test-file)))
  (write-test-strings-to-file testfile file-strings)
  (handler-case 
      (progn (parse-fasta-file testfile)  2)
    (error () 0)
    (warning () 1)
    ))

(defun write-fasta-file-and-test-output
       (file-strings 
        &key 
        (testfile (fasta-test-file))
        (function #'fasta-index-datum-key))
  (write-test-strings-to-file testfile file-strings)
  (mapcar function (fasta-index-data (parse-fasta-file testfile))))
  
(defun write-fasta-file-and-find-fasta-record
       (keys file-strings &key (testfile (fasta-test-file)))
  (write-test-strings-to-file testfile file-strings)
  (with-fasta-db (db testfile) 
    (mapcar #'(lambda (key) (second (find-fasta-record db key))) keys)
    ))

(defun write-fasta-file-and-find-fasta-header
       (keys file-strings &key (testfile (fasta-test-file)))
  (write-test-strings-to-file testfile file-strings)
  (with-fasta-db (db testfile) 
    (mapcar #'(lambda (key) (find-fasta-header db key)) keys)
    ))

(defun write-fasta-file-and-find-fasta-subsequence
       (key start end direction file-strings 
            &key (testfile (fasta-test-file)))
  (write-test-strings-to-file testfile file-strings)
  (with-fasta-db (db testfile)
    ;; Insure only 1 value is returned.
    (let ((result (find-fasta-subsequence db key start end direction)))
      result
      )))

(defun write-fasta-file-and-find-fasta-subsequences
       (key-start-end-direction-list file-strings 
            &key (testfile (fasta-test-file)))
  (write-test-strings-to-file testfile file-strings)
  (with-fasta-db (db testfile)
    (loop for (key start end direction) in key-start-end-direction-list
          collect
          ;; Insure only 1 value is returned.
          (let ((result (find-fasta-subsequence db key start end direction)))
            result
            ))))

(defun list-string= (s1-list s2-list)
  (every #'(lambda (s1 s2) (string= s1 s2)) s1-list s2-list))


(defun file-in-loaddir (file) (fasta-test-file file))

(defun ffile1 () (file-in-loaddir "sample1.fsta"))
(defun ffile2 () (file-in-loaddir "sample2.fsta"))
(defun ffile3 () (file-in-loaddir "sample3.fsta"))
(defun db1 () (file-in-loaddir "db1.fdbs"))
(defun db2 () (file-in-loaddir "db2.fdbs"))
(defun db3 () (file-in-loaddir "db3.fdbs"))

(defun purge-derived-fasta-files (&rest fasta-source-files)
  (when (not (listp fasta-source-files))
    (setq fasta-source-files (list fasta-source-files)))
  (dolist (fasta-source-file fasta-source-files)
    (let* ((path (pathname fasta-source-file))
           (ipath (pathname-of-index-ft path))
           (cpath (pathname-of-compiled-index-ft path)))
      (delete-no-error ipath)
      (delete-no-error cpath)
      )))

(defun purge-fastadb-files (fastadb-file &optional (delete-dbfile nil))
  (let* ((path (pathname fastadb-file))
         (cpath (pathname-of-compiled-database-ft path)))
    (when (probe-file cpath) (delete-no-error cpath))
    (when delete-dbfile (delete-no-error path))
    ))

(defun fasta-t1 (&key (testdriver nil))
  (purge-derived-fasta-files (ffile1))
  ;; Should create the index file and compile it and load it.
  (with-fasta-db (dbh (ffile1))
    (let ((q1 (find-fasta-record dbh "two-gene"))
          (q2 (find-fasta-record dbh "Not there"))
          (q3 (find-fasta-record dbh "four-gene")))
      (if testdriver
          (list q1 q2 q3)
        (progn
          (format t "~&Two-gene: ~S~%" q1)
          (format t "~&Not there: ~S~%" q2)
          (format t "~&Four-gene (no data): ~S~%" q3)
          (values))))))

(defun fasta-t2 (&key (testdriver nil))
  (purge-derived-fasta-files (ffile2))
  ;; Should create the index file and compile it and load it.
  (with-fasta-db (dbh (ffile2))
    (let ((q1 (find-fasta-record dbh "x-gene"))
          (q2 (find-fasta-record dbh "unknown-gene")))
      (if testdriver
          (list q1 q2)
        (progn
          (format t "~&X gene: ~S~%" q1)
          (format t "~&Unknown gene: ~S~%" q2)
          (values))))))

(defun fasta-t3 (&key (testdriver nil))
  ;; Wipe out old test merged db.
  (purge-fastadb-files (db1) t)
  ;; Should create the db file and compile it.
  (create-fasta-db (db1) (list (ffile1) (ffile2)))
  ;; Should load the compiled db file.
  (with-fasta-db (dbh (db1))
    ;; Data from both should be accessible using new db.
    (let ((q1 (find-fasta-record dbh "two-gene"))
          (q2 (find-fasta-record dbh "Not there"))
          (q3 (find-fasta-record dbh "four-gene"))
          (q4 (find-fasta-record dbh "x-gene"))
          (q5 (find-fasta-record dbh "unknown-gene")))
      (if testdriver
          (list q1 q2 q3 q4 q5)
        (progn
          (format t "~&Two gene: ~S~%" q1)
          (format t "~&Not there: ~S~%" q2)
          (format t "~&Four gene (no data): ~S~%" q3)
          (format t "~&X gene: ~S~%" q4)
          (format t "~&Unknown gene: ~S~%" q5)
          (values))))))


(defun fasta-t4 (&key (testdriver nil))
  ;; Test that we can merge a DB with a fasta file.
  (purge-fastadb-files (db2) t)
  (purge-derived-fasta-files (ffile3))
  (create-fasta-db (db2) (list (db1) (ffile3)))
  (with-fasta-db (dbh (db2))
    (let ((q1 (find-fasta-record dbh "two-gene"))
          (q2 (find-fasta-record dbh "x-gene"))
          (q3 (find-fasta-record dbh "hair-color-gene")))
      (if testdriver
          (list q1 q2 q3)
        (progn
          (format t "~&Two gene: ~S~%" q1)
          (format t "~&X gene: ~S~%" q2)    
          (format t "~&Hair gene: ~S~%" q3)
          (values))))))


(defun fasta-t5 (&key (testdriver nil))
  ;; And the other way around, just in case.
  (purge-fastadb-files (db2) t)
  (purge-derived-fasta-files (ffile3))
  (create-fasta-db (db2) (list (ffile3) (db1)))
  (with-fasta-db (dbh (db2))
    (let ((q1 (find-fasta-record dbh "two-gene"))
          (q2 (find-fasta-record dbh "x-gene"))
          (q3 (find-fasta-record dbh "hair-color-gene")))
      (if testdriver
          (list q1 q2 q3)
        (progn
          (format t "~&Two gene: ~S~%" q1)
          (format t "~&X gene: ~S~%" q2)    
          (format t "~&Hair gene: ~S~%" q3)
          (values))))))

(defun fasta-t6 (&key (testdriver nil))
  ;; And now create two DB files, using (FFILE1) & (FFILE2), 
  ;; then (FFILE2) & (FFILE3) then merge them together.  
  ;; (Should see duplicate file note)
  (purge-derived-fasta-files (ffile1) (ffile2) (ffile3))
  (purge-fastadb-files (db1) t)
  (purge-fastadb-files (db2) t)
  (purge-fastadb-files (db3) t)
  (create-fasta-db (db1) (list (ffile1) (ffile2)))
  (create-fasta-db (db2) (list (ffile2) (ffile3)))
  (create-fasta-db (db3) (list (db1) (db2)))
  ;; Data from each FASTA file should be present!
  (with-fasta-db (dbh (db3))
    (let ((q1 (find-fasta-record dbh "two-gene"))
          (q2  (find-fasta-record dbh "x-gene"))
          (q3 (find-fasta-record dbh "hair-color-gene")))
      (if testdriver
          (list q1 q2 q3)
        (progn
          (format t "~&Two gene: ~S~%" q1)
          (format t "~&X gene: ~S~%" q2)    
          (format t "~&Hair gene: ~S~%" q3)
          (values))))))

(defun gene-name (query-result)
  (and query-result (first (string-split (first query-result) #\Space))))
