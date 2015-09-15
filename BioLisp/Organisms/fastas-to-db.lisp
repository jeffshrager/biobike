;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

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

;;; Author: JP Massar


(defun process-contig-fastas (orgn fasta-files &optional protein-dir)

  ;; Turn the fasta files for the contig's of an organism
  ;; into a single long sequence file with an associated index file.
  (let ((seqdir (if protein-dir protein-dir (seqinfo-directory orgn))))
    (cformatt "Storing FASTA data into SEQ files in directory ~A" seqdir)
    (handler-case
        (ensure-directories-exist seqdir :verbose t)
      (error
       ()
       (formatn
        (one-string
         "Cannot access or cannot create the directory to store "
         "sequence information for organism ~A. "
         "The inaccessible directory is ~A. ")
        orgn seqdir
        )))
    ;; Create sequence.seqidx and sequence.seq files
    (contig-fasta-files-to-index-and-seq-files 
     fasta-files seqdir "sequence" :verbose? t)
    (cformatt "FASTA data stored into SEQ files in directory ~A" seqdir)
    ))


(defun contig-fasta-files-to-index-and-seq-files 

       (fasta-files result-dir result-file-name &key (verbose? t))

  (ensure-directories-exist result-dir :verbose t)

  (let*  ((fasta-paths (mapcar #'merge-pathnames fasta-files))
          (index-file 
           (merge-pathnames 
            (make-pathname :name result-file-name :type *seqinfo-index-ext*)
            result-dir))
          (seq-file-path
           (merge-pathnames 
            (make-pathname :name result-file-name :type *seqinfo-ext*)
            result-dir))
          (index-list nil)
          (start-header 0)
          (start-data 0)
          (found-non-blank-line? nil)
          (reading-data? nil)
          (file-header-count 0)
          (linecount 0)
          (charcount 0)
          (key nil)
          (contig-prefix (organism-element-prefix :contig))
          )

    (when verbose?
      (cformatt "Creating sequence file ~A" (namestring seq-file-path))
      (cformatt "Creating index file ~A" (namestring index-file))
      (cformatt "Using fasta files: ")
      (dolist (f fasta-paths) (cformatt "  ~A" (namestring f))))
                
    (flet ((add-entry ()
             (push 
              (list (one-string contig-prefix key)
                    start-header start-data (- charcount start-data))
              index-list)))

      ;; Open the output sequence file

      (with-open-file 
          (seqp seq-file-path :direction :output :if-exists :supersede)

        ;; For each input file...

        (dolist (fasta-file fasta-paths)

          ;; Get rid of any %$#^&** RETURN/NEWLINE sequences

          (strip-file-of-returns-preceding-newlines fasta-file)

          ;; Open it

          (when verbose? 
            (cformatt "Processing fasta file ~A" (namestring fasta-file)))

          (with-open-file (fastap fasta-file :direction :input)

            ;; Set up state for this file.

            (setq reading-data? nil)
            (setq found-non-blank-line? nil)
            (setq file-header-count 0)
            (setq linecount 0)

            ;; Read successive lines.

            (do ((line (read-line fastap nil nil) (read-line fastap nil nil)))
                ((null line))

              (incf linecount)

              ;; Skip over blank lines.  Warn if blank lines found
              ;; in middle of file.

              (if (every 
                   #'(lambda (ch) (or (eql ch #\Space) (eql ch #\Tab))) 
                   line)

                  (when found-non-blank-line?
                    (warn 
                     (one-string
                     "File ~A.  Line ~D~% "
                     "Blank line found in middle of fasta file.")
                     (namestring fasta-file) linecount))

                ;; Process a non-blank line.

                (progn
                  (setq found-non-blank-line? t)
                  (write-sequence line seqp)
                  (cond
                   ((eql (char line 0) #\>)
                    (when (and (plusp file-header-count) (not reading-data?))
                      (error 
                       "File ~A.  Line ~D~%No data found after header: ~S"
                       (namestring fasta-file) linecount line))
                    (when (plusp file-header-count) (add-entry))
                    (incf file-header-count)
                    (setq reading-data? nil)
                    (setq key (get-contig-fasta-key line))
                    (setq start-header (1+ charcount))
                    (setq start-data (+ charcount (length line))))
                   (t 
                    (setq reading-data? t)
                    (unless (every 'alpha-char-p line)
                      (error 
                       "File ~A, Line ~D, non-alphabetic character on line"
                       (namestring fasta-file) linecount
                       ))))
                  (incf charcount (length line))
                  )))

            ;; Add the last record.

            (unless reading-data? (error "No data found after last header."))
            (add-entry)

            )

          ;; End processing of this file
          ;; Process next file.

          (cformatt "~D records processed" file-header-count)

          ))

      ;; All done.  Output sequence file has been closed.

      (when verbose? 
        (cformatt "All fasta files processed.  Checking for duplicate keys."))

      (setq index-list (nreverse index-list))
      (let ((duplicates
             (check-for-duplicates 
              index-list :key 'first :test 'string-equal)))
        (when duplicates
          (error "*** Ruh roh.  Duplicates keys found: ~S" duplicates)))
        
      ;; Open the output index file and write out the index info.

      (when verbose?
        (cformatt 
         "Writing index data (~D entries) to index file ~A" 
         (length index-list) (namestring index-file)))

      (with-open-file (p index-file :direction :output :if-exists :supersede)
        (format p "~S" index-list))

      (when verbose? (cformatt "Processing complete."))

      index-list
      (print (list 'charcount charcount))

      )))


(defun get-contig-fasta-key (line)
  (let ((pos (position-if
              #'(lambda (x) (or (eql x #\Space) (eql x #\Tab) (eql x #\,)))
              line)))
    (if pos (subseq line 1 pos) (subseq line 1))))



;; Take a set of FASTA files and store the information in them
;; into a table in a MYSQL database.


(defun fastas-in-subdir-to-db 
       (fasta-files database-table-name key-prefix
                    &optional (data-element-predicate 'identity))

  (cformatt "Storing FASTA data into MYSQL table ~A" database-table-name)
  (loop for file in fasta-files do
        (cformatt "  Using FASTA file ~A" (namestring file)))

  ;; Create the table to hold the FASTA data.

  (cformatt "Creating MYSQL table ~A." database-table-name)
  (create-fasta-storage-table database-table-name)
  (push database-table-name *new-organism-tables*)
  (cformatt "Created MYSQL table ~A." database-table-name)

  (let* ((temp-fdbs (merge-pathnames "temp.fdbs" (first fasta-files))))

    ;; Create a single FASTA DB out of all of the FASTA files so
    ;; we can easily iterate over all the entries.

    (cformatt "Creating temporary FASTA DB from FASTA files.")

    (create-fasta-db 
     temp-fdbs fasta-files :if-exists :supersede :verbose t)

    (cformatt 
     (one-string "Verifying that fasta keys are not too long and creating "
                 "a list of all the keys."))
    (setq *fasta-keys* nil)
    (let ((invalid-keys nil))
      (with-fasta-db (db temp-fdbs)
        (with-fasta-db-keys (key db)
          (setq key (one-string key-prefix key))
          (push key *fasta-keys*)
          (when (> (length key) *standard-key-max-length*)
            (push key invalid-keys))))
      (when invalid-keys
        (cformatt "Keys that are too long exist!!")
        (dolist (key invalid-keys) (cformatt "  Invalid key: ~A" key))
        (return-from fastas-in-subdir-to-db nil)
        ))
    (setq *fasta-keys* (nreverse *fasta-keys*))

    (cformatt "Moving FASTA data to MYSQL table ~A" database-table-name)

    ;; This runs into Allegro's 16 meg limit on size of an array...

    (unwind-protect
        (fasta-db-to-sql-via-load-data-infile 
         temp-fdbs database-table-name key-prefix data-element-predicate)
      (delete-file temp-fdbs)
      )

    (cformatt "Storage of fasta data complete.")

    ))


#|  FROM THE MYSQL SPECIFICATION

In other words, the defaults cause LOAD DATA INFILE to act as follows
when reading input:

    * Look for line boundaries at newlines.
    * Break lines into fields at tabs.
    * Do not expect fields to be enclosed within any quoting
characters.  
    * Interpret occurrences of tab, newline, or `\' preceded by `\' as
literal characters that are part of field values.

|#

;; Write a sequence to the SQL load file, making sure to escape
;; characters that need escaping according to the above spec.

(defun output-load-data-infile-string (s p)
  (flet ((char-needs-escape? (ch)
           (or (eql ch #\Tab) (eql ch #\Newline) (eql ch #\\))))
    (let ((easy t))
      (loop for ch across s do
            (when (char-needs-escape? ch) (setq easy nil) (return)))
    (if easy
        (write-sequence s p)
      (loop for ch across s do
            (when (char-needs-escape? ch) (write-char #\\ p))
            (write-char ch p)
            )))))

(defun fasta-db-to-sql-via-load-data-infile 
       (temp-fdbs database-table-name key-prefix
                  &optional (data-element-predicate 'identity))
  (let ((count 0)
        (temp-file 
         (namestring 
          (merge-pathnames
           (make-pathname :name "temp-load-data-infile" :type "temp")
           *tmp-directory*
           ))))
    (cformatt "Creating temporary SQL load file: ~A" temp-file)
    (unwind-protect
        (progn
          (with-open-file 
              (p temp-file :direction :output :if-exists :supersede)
            (with-fasta-db (db temp-fdbs)
              (with-fasta-db-keys (key db)
                (destructuring-bind (header data)
                    (find-fasta-record db key)
                  (let ((trimmed-data (string-trim " " data)))
                    (unless (= (length data) (length trimmed-data))
                      (cformatt 
                       "*** Warning: Spaces trimmed on data for ~D"
                       header)
                      (setq data trimmed-data)
                      ))
                  (unless (every data-element-predicate data)
                    (error
                     (one-string
                      "Invalid fasta data. "
                      "Does not satisfy data element predicate.~%"
                      "Fasta header: ~A~%"
                      "Data line in violation: ~A")
                     header data))
                  (setq key (one-string key-prefix key))
                  (output-load-data-infile-string key p) 
                  (write-char #\Tab p)
                  (output-load-data-infile-string header p) 
                  (write-char #\Tab p)
                  (output-load-data-infile-string data p) 
                  (write-char #\Tab p)
                  (format p "~D" (length data)) 
                  (write-char #\Newline p)
                  (incf count)
                  ))))
          (cformatt "Doing UPLOAD from file into SQL")
          (upload-fasta-data-via-load-data-infile 
           database-table-name temp-file)
          (cformatt "UPLOAD complete.  Deleting temp file.")
          )
      (when (probe-file temp-file) (delete-file temp-file)))
    (cformatt "~D FASTA data records stored in database." count))
  )


#+test
(defun test-fasta-store ()

  (let* ((organism "mutant_ninja_turtles")
	 (ottn (organism-tables-table-name organism))
         (fasta-db-table-name "mutant_ninja_turtles_genome")
         (data1
          (one-string-nl
           ">key1 a random header."
           "acgttgca"
           "aaaaaaaaaaaaaaaaaaa"
           ">key2 another random header"
           "ccccccccccccccccccccc"
           "ggggggggggggggg"
           "ttttttt"
           ))
         (data2
          (one-string-nl
           ">key3 a non-random header."
           "acgttgca"
           "aaaaaaaaaaaaaaaaaaa"
           "acgttgca"
           ">key4 another non-random header"
           "aaa"
           "ccccccccccccccccccccc"
           "ggggggggggggggg"
           "ttttttt"
           ))
         (data-list (list data1 data2))
	 (file-list (list "data1" "data2"))
         (files 
          (mapcar 
	   #'(lambda (name) (one-string "biotmp:" name ".fasta"))
	   file-list
	   )))

    ;; Create the test fasta files

    (loop for file in files 
          for data in data-list do
          (with-open-file (p file :direction :output :if-exists :supersede)
            (format p "~A~%" data)))

    ;; Put the data from the test fasta files into the MYSQL db.

    (fastas-to-db 
     organism
     files
     fasta-db-table-name
     )

    ;; Read some of the data out to verify that it is there.

    (with-organisms-db
        (db)
      (formatt
       "Organisms list: ~A~%"
       (esql "select name from organisms"))
      (formatt
       "Tables about organism list: ~A~%"
       (esql (formatn "select table_name from ~A" ottn)))
      (formatt
       "Test key names: ~A~%"
       (esql (formatn "select name from ~A" fasta-db-table-name)))
      (formatt
       "Key1 sequence: ~A~%"
       (esql 
        (formatn 
         "select sequence from ~A where name = 'key1'"
         fasta-db-table-name
         )))

      ;; Delete the table we created and the info about that table
      ;; (The fact that the organism exists, and the fact that
      ;; a table holding data for that organism exists)

      (delete-organism-from-db organism)

      )

    ))
         

