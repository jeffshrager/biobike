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

;;; Author:  JP Massar

;;; Create little functions that will load each organism.

(defmacro upload-call (fname orgn)
  `(defun ,fname ()
     (upload-new-organism 
      ,orgn
      (organism-data-directory ,orgn)
      :db-execute ,(ecase (os?) (:unix t) (:windows nil))
      )))

;; Organism upload functions in alphabetical order.

(upload-call uap "anabaena_pcc7120")
(upload-call uav "anabaena_variabilis_atcc29413")
(upload-call ucw "crocosphaera_watsonii_wh8501")
(upload-call ugv "gloeobacter_violaceus_pcc7421")
(upload-call unp "nostoc_punctiforme_atcc29133")
(upload-call upm4 "prochlorococcus_marinus_med4")
(upload-call upm9313 "prochlorococcus_marinus_mit9313")
(upload-call upm "prochlorococcus_marinus_ss120")
(upload-call use "synechococcus_elongatus_pcc7942")
(upload-call usw "synechococcus_wh8102")
(upload-call usp "synechocystis_pcc6803")
(upload-call utb "thermosynechococcus_elongatus_bp1")
(upload-call ute "trichodesmium_erythraeum")


;;;; Code dealing with uploading a new organism into the ORGANISMS database.

;;;; We are given a path to a directory and the organism's name.

;;;; Step 1.  Verify that the directory is consistent with the
;;;; specification of how the directory should be set up, and
;;;; create a 'catalog' of what needs to be processed.

;;;; Step 2.  If organism data for this organism already exists
;;;; in the database, save it away by renaming the old tables, and
;;;; saving the entry for the organism in the ORGANISMS table.

;;;; Step 3.  Go through each subdirectory of the new organism's
;;;; directory and create appropriate database tables using the
;;;; files in the subdirectory.  Generally this will involve creating
;;;; a single FASTA database table containing information from
;;;; multiple fasta files in the subdirectory, and creating a single
;;;; INFORMATION database table containing stuff from the .tbl file
;;;; in the subdirectory.  Keep a list of all the tables created.

;;;; Step 4.  Create a new entry in the ORGANISMS database table
;;;; for this organism, deleting the old entry if it exists, and
;;;; also create the 'tables table' for this organism, a list of
;;;; all the tables that were just created.

;;;; Step 5.  If the entire process is successful, delete the old
;;;; organism tables, otherwise delete the new tables and restore
;;;; the old organism tables and the old organism entry.


(defun upload-new-organism 
       (organism organism-directory &key (db-execute t))
  (setq organism (lcstring organism))
  (setq organism-directory 
        (lcstring (namestring (pathname organism-directory))))
  (cformatt "Processing organism distribution for organism ~A" organism)
  (cformatt "Organism name: ~A" organism)
  (cformatt "Submission directory: ~A" organism-directory)
  (unless (probe-file organism-directory)
    (error "Submission directory does not exist!!"))
  (let ((*db-execute* db-execute))
    (upload-new-organism-internal organism organism-directory)))

(defun upload-new-organism-internal (organism organism-directory)

  ;; STEP 1

  (cformatt "Checking for required and optional files and directories...")
  (let ((od (truename (pathname organism-directory)))
        organism-plist
        genome-dir genome-dir-exists? genome-ok? genome-fastas genome-tbl
        genes-dir genes-dir-exists? genes-ok? genes-fastas genes-tbl
        transcripts-dir transcripts-dir-exists? transcripts-ok?
        transcripts-fastas transcripts-tbl
        proteins-dir proteins-dir-exists? proteins-ok?
        proteins-fastas proteins-tbl
        other-dir other-dir-exists? other-ok? other-fastas other-tbls
        documentation-dir documentation-dir-exists? 
        documentation-ok? test-file
        (*db-verbose* nil)
        (*upload-organism* organism)
        (*fasta-keys* nil)
        (*genome-keys* nil)
        (*genes-keys* nil)
        (*transcripts-keys* nil)
        (*proteins-keys* nil)
        )
    (declare (ignore genome-dir-exists? documentation-ok?))
    (cformatt "Verifying organism property list file")
    (setq organism-plist (verify-organism-plist-file organism od))
    (cformatt "Organism property list file verified.")
    (flet ((subdir (name) (merge-pathnames (one-string name "/") od)))
      (macrolet ((setdir (symbol name) `(setq ,symbol (subdir ,name))))
        (setdir genome-dir "genome")
        (setdir genes-dir "genes")
        (setdir transcripts-dir "transcripts")
        (setdir proteins-dir "proteins")
        (setdir other-dir "other")
        (setdir documentation-dir "documentation")
        ))
    (cformatt "Verifying genome directory")
    (multiple-value-setq 
        (genome-dir-exists? genome-ok? genome-fastas genome-tbl)
        (verify-organism-genome-directory genome-dir))
    (setq genome-tbl (first genome-tbl))
    (cformatt "Verifying genes directory")
    (multiple-value-setq (genes-dir-exists? genes-ok? genes-fastas genes-tbl)
        (verify-organism-genes-directory genes-dir))
    (setq genes-tbl (first genes-tbl))
    (cformatt "Verifying transcripts directory")
    (multiple-value-setq 
        (transcripts-dir-exists? transcripts-ok?
                                 transcripts-fastas transcripts-tbl)
        (verify-organism-transcripts-directory transcripts-dir))
    (setq transcripts-tbl (first transcripts-tbl))
    (cformatt "Verifying proteins directory")
    (multiple-value-setq 
        (proteins-dir-exists? proteins-ok? proteins-fastas proteins-tbl)
        (verify-organism-proteins-directory proteins-dir))
    (setq proteins-tbl (first proteins-tbl))
    (cformatt "Verifying other directory")
    (multiple-value-setq (other-dir-exists? other-ok? other-fastas other-tbls)
        (verify-organism-other-directory other-dir))

    (unless (and genome-ok? genes-ok? transcripts-ok? proteins-ok? other-ok?)
      (cformatt "Upload terminated because one or more subdirectories invalid")
      (return-from upload-new-organism-internal  nil))

    (cformatt "Verifying documentation directory")
    (multiple-value-setq (documentation-dir-exists? test-file)
        (verify-organism-documentation-directory documentation-dir))

    (cformatt "All subdirectories verified.  Status:")
    (cformatt "  Organism properties read:")
    (loop for (key value) in organism-plist do
          (cformatt "    ~A: ~S" key value))
    (cformatt 
     "  Genome dir ok?: ~A, number of fastas: ~D, table file found: ~A"
     genome-ok? (length genome-fastas) (not (null genome-tbl)))
    (when genes-dir-exists?
      (cformatt 
       "  Genes dir ok?: ~A, number of fastas: ~D, table file found: ~A"
       genes-ok? (length genes-fastas) (not (null genes-tbl))))
    (when transcripts-dir-exists?
      (cformatt 
       "  Transcripts dir ok?: ~A, number of fastas: ~D, table file found: ~A"
       transcripts-ok? 
       (length transcripts-fastas)
       (not (null transcripts-tbl))))
    (when proteins-dir-exists?
      (cformatt 
       "  Proteins dir ok?: ~A, number of fastas: ~D, table file found: ~A"
       proteins-ok? (length proteins-fastas) (not (null proteins-tbl))))
    (when other-dir-exists?
      (cformatt 
       "  Other dir ok?: ~A, number of fastas: ~D, number of tbls: ~D"
       other-ok? (length other-fastas) (length other-tbls)))
    (when documentation-dir-exists?
      (cformatt "  Test file in doc directory: ~A" (not (null test-file))))

    (with-organisms-db (db)

      ;; STEP 2

      (cformatt "Renaming old organism tables, if any.")
      (save-old-organism-tables organism)
      (cformatt "Old organism tables, if any, renamed.")

      (let* ((success? nil) 
             (*new-organism-tables* nil)
             (*organism-plist* organism-plist)
             )

        (cformatt "Organism prefix: ~A" (organism-element-prefix :organism))
        (cformatt "Contig prefix: ~A" (organism-element-prefix :contig))
        (cformatt "Genes prefix: ~A" (organism-element-prefix :genes))
        (cformatt "Proteins prefix: ~A" (organism-element-prefix :proteins))

        (unwind-protect

            (progn

              ;; STEP 3

              (cformatt "Processing genome directory")
              (do-the-genome genome-fastas genome-tbl)
              (when genes-dir-exists? 
                (cformatt "Processing genes directory")
                (do-the-genes genes-tbl))
              (when transcripts-dir-exists?
                (cformatt "Processing transcripts directory")
                (do-the-transcripts transcripts-fastas transcripts-tbl))
              (when proteins-dir-exists?
                (cformatt "Processing proteins directory")
                (do-the-proteins proteins-fastas proteins-tbl))
              (when other-dir-exists?
                (cformatt "Processing other directory")
                (do-the-other other-fastas other-tbls))
              (when documentation-dir-exists?
                (cformatt "Processing documentation directory")
                (do-the-documentation test-file))
              (cformatt "Creating organisms table if not already existing")
              (maybe-create-organisms-table)

              ;; STEP 4

              (cformatt "Creating new tables table.")
              (create-organism-tables-table
               (organism-tables-table-name organism) *new-organism-tables*)
              (cformatt 
               "Adding organism and its properties to ~A"
               *organisms-table-name*)
              (add-organism-to-organisms-table organism organism-plist)
              (setq success? t)
              )

          ;; STEP 5

          (if success?
              (progn
                (cformatt "Deleting old tables, if any")
                (delete-old-organism-tables)
                (cformatt "Upload complete!"))
            (progn
              (cformatt "Problem detected in upload process.")
              (cformatt "Deleting any new tables that were created.")
              (delete-new-organism-tables)
              (cformatt "Restoring old tables and old properties, if any")
              (restore-old-organism-tables organism)
              (cformatt "Upload terminated unsuccessfully.")
              ))))

      )))


(defun save-old-organism-tables (organism-name)
  (setq *saved-old-organism-tables* nil)
  (setq *saved-organism-entry* nil)
  (let ((all-tables (all-database-table-names)))
    (when (member *organisms-table-name* all-tables :test 'string-equal)
      (let ((tables-to-save (tables-belonging-to-organism organism-name))
            (*db-verbose* t))
        (dolist (table-name tables-to-save)
          (let ((saved-table-name 
                 (one-string *saved-table-prefix* table-name)))
            (rename-database-table table-name saved-table-name)
            (push saved-table-name *saved-old-organism-tables*)
            )))
      (setq *saved-organism-entry* (retrieve-organism-info organism-name)))))


(defun restore-old-organism-tables (organism)
  (let ((len (length *saved-table-prefix*))
        (all-tables (all-database-table-names))
        (*db-verbose* t))
    ;; Restore all the old tables
    (dolist (table-name *saved-old-organism-tables*)
      (let ((original-table-name (subseq table-name len)))
        (when (find table-name all-tables :test #'string=)
          (delete-database-table table-name))
        (rename-database-table table-name original-table-name)
        )))
  ;; Put back the original property list in the ORGANISMS table.
  (when *saved-organism-entry*
    (let* ((saved-plist-string (second *saved-organism-entry*))
           (saved-description (third *saved-organism-entry*)))
      (insert-into-organisms-table 
       organism saved-plist-string saved-description)
      )))
    
(defun delete-old-organism-tables ()
  (dolist (table-name *saved-old-organism-tables*)
    (delete-database-table table-name)))
             
(defun delete-new-organism-tables ()
  (dolist (table-name *new-organism-tables*)
    (delete-database-table table-name)))

(defun maybe-create-organisms-table ()
  (verify-table-existence 
   (all-database-table-names) *organisms-table-name* 'create-organisms-table
   ))

(defun create-organism-tables-table (table-name &optional entries)
  (let ((tables-table-name (lcstring table-name)))
    (create-tables-table tables-table-name)
    (dolist (entry entries)
      (insert-into-tables-table tables-table-name entry))
    (push tables-table-name *new-organism-tables*)
    ))
  

(defun delete-organism-from-organisms-table (organism)
  (esql
   (formatn
    "delete from ~A where ~A = '~A'"
    *organisms-table-name* *organisms-table-key* organism
    )))

(defun add-organism-to-organisms-table (organism property-list)
  ;; Update the organisms table, adding the organism and its property list.
  ;; (Delete it and then add it to make sure we don't create
  ;; duplicate rows)
  (cformatt 
   "Updating table ~A with new organism information"
   *organisms-table-name*)
  (delete-organism-from-organisms-table organism)
  ;; Pull out the DESCRIPTION property, if any, from the PLIST.
  (let ((description (cadr (assoc :description property-list))))
    (setq property-list (delete :description property-list :key #'car))
    (insert-into-organisms-table 
     organism (plist-to-plist-string property-list) (or description "")))
  (cformatt "~A table updated." *organisms-table-name*)
  )

        
(defun verify-standard-organism-directory 
       (standard-dir 
        dirname 
        &key 
        ;; MUST-EXIST, MAX-EXIST
        (directory-status :may-exist)
        ;; MUST-EXIST, MAX-EXIST, MUST-NOT-EXIST
        (fastas-status :must-exist) 
        ;; NONE, EXACTLY-ONE, ANY-NUMBER
        (tbl-status :exactly-one)
        ;; ERROR, OK
        (unknown-status :error)
        )
  (unless (probe-file standard-dir)
    (when (eq directory-status :must-exist)
      (error "The ~A directory, ~A, must exist, but does not!"
             dirname (namestring standard-dir)
             ))
    (return-from verify-standard-organism-directory 
      (values nil t nil nil nil)))
  (let ((all-files (directory-of-dir standard-dir))
        (fasta-files nil)
        (table-files nil)
        (unknown-files nil)
        (oops nil))
    (dolist (file all-files)
      (cond
       ((string-equal (pathname-type file) "fasta") (push file fasta-files))
       ((string-equal (pathname-type file) "tbl") (push file table-files))
       (t (push file unknown-files))
       ))
    (when (and unknown-files (eq unknown-status :error))
      (dolist (file unknown-files)
        (cond
         ;; Don't crap out because of a previous run.
         ((member (pathname-type file) 
                  *generated-temp-file-types* :test #'string-equal)
         (cformatt 
          (one-string
           "Deleting file ~A, presumably created from a previously "
           "unsuccessful upload attempt.")
           (namestring file))
          (delete-file file))
         (t
          (setq oops t)
          (cformatt 
           "*** The file ~A should not be in the ~A directory" 
           file dirname)))))
    (when (and (eq fastas-status :must-exist) (null fasta-files))
      (setq oops t)
      (cformatt "*Ruh roh. No FASTA files exist in the ~A directory" dirname))
    (when (and (eq fastas-status :must-not-exist) fasta-files)
      (setq oops t)
      (cformatt "*Ruh roh. FASTA files exist in the ~A directory" dirname))
    (when (and (null table-files) (eq tbl-status :exactly-one))
      (setq oops t)
      (cformatt "*Ruh roh. No TABLE file exists in the ~A directory" dirname))
    (when (and (> (length table-files) 0) (eq tbl-status :none))
      (setq oops t)
      (cformatt "*Ruh roh. A table file exists in the ~A directory" dirname))
    (when (and (/= (length table-files) 1) (eq tbl-status :exactly-one))
      (setq oops t)
      (cformatt 
       "*Ruh roh. Either no TABLE file or more than one exists in ~A" dirname))
    (values t (not oops) fasta-files table-files unknown-files)
    ))


(defun verify-organism-genome-directory (genome-dir)
  (verify-standard-organism-directory 
   genome-dir "GENOME" :directory-status :may-exist))
(defun verify-organism-genes-directory (genes-dir)
  (verify-standard-organism-directory
   genes-dir "GENES" :fastas-status :must-not-exist))
(defun verify-organism-transcripts-directory (transcripts-dir)
  (verify-standard-organism-directory 
   transcripts-dir "TRANSCRIPTS" :fastas-status :may-exist))
(defun verify-organism-proteins-directory (proteins-dir)
  (verify-standard-organism-directory 
   proteins-dir "PROTEINS" :fastas-status :may-exist))
(defun verify-organism-other-directory (other-dir)
  (verify-standard-organism-directory
   other-dir "OTHER" :fastas-status :may-exist :tbl-status :any-number
   ))

(defun verify-organism-documentation-directory (docdir)
  (let ((test-file (merge-pathnames *organisms-seq-testfile* docdir)))
    (cond
     ((null (probe-file docdir)) nil)
     ((null (probe-file test-file)) (values t nil))
     (t (values t test-file))
     )))
    


(defun do-the-genome (genome-fastas genome-tbl)
  (cformatt "Processing genome fasta files")
  (process-contig-fastas *upload-organism* genome-fastas)
  (setq *genome-keys* 
        (mapcar 'seqidx-key (organism-seqidx-info *upload-organism*)))
  (cformatt "Processing genome tbl file")
  (unless (genome-tbl-in-subdir-to-db 
           genome-tbl (genome-info-tbl *upload-organism*))
    (error "Genome tbl file processing failed. Cannot continue."))
  )


(defun do-the-genes (genes-tbl)
  (unless (genes-tbl-in-subdir-to-db 
           genes-tbl (genes-info-tbl *upload-organism*))
    (error "Genes tbl file processing failed. Cannot continue.")))


(defun do-the-transcripts (transcripts-fastas transcripts-tbl)
  (declare (ignore transcripts-fastas transcripts-tbl))
  (cformatt "Transcripts processing not implemented.")
  t
  )

(defun valid-protein-char? (ch) (not (eql ch #\Space)))

(defun do-the-proteins (protein-fastas protein-tbl)
  (fastas-in-subdir-to-db
   protein-fastas 
   (proteins-seq-tbl *upload-organism*)
   (organism-element-prefix :proteins)
   'valid-protein-char?)
  (unless (proteins-tbl-in-subdir-to-db
           protein-tbl
           (proteins-info-tbl *upload-organism*)
           (genes-info-tbl *upload-organism*)
           )
    (error "Proteins tbl file processing failed. Cannot continue.")
    ))
                           

(defun do-the-other (other-fastas other-tbls)
  (declare (ignore other-tbls))
  (dolist (fasta-file other-fastas)
    (fastas-in-subdir-to-db 
     (list fasta-file) (other-sequences-table-name fasta-file) ""
     )))


(defun do-the-documentation (test-file)
  (declare (ignore test-file))
  t
  )
      


(defun clean-up-database-tables 
       (&optional 
        (matchf #'(lambda (x) (eql 0 (search *saved-table-prefix* x)))))
  (with-organisms-db (db)
    (dolist (table-name (all-database-table-names))
      (when (funcall matchf table-name)
        (delete-database-table table-name)
        ))))


;;; DELETE-ORGANISM-FROM-DB

;;; Delete all tables from the DB that are listed as having data
;;; belong solely to the organism in question, along with the table
;;; that holds this information.  Also delete the
;;; organism's name from the list of organisms stored in the 'organisms'
;;; database table.

;;; This does NOT delete data associated with an organism found in
;;; other tables, such as a crossmetrics table.

(defun delete-organism-from-db (organism &key (verbose? t))
  (with-organisms-db (db)
    (when verbose? 
      (cformatt "Deleting organism ~A from database." organism))
    (let* ((organism-name (lcstring organism))
           (ottn (organism-tables-table-name organism-name))
           (existing-tables (all-database-table-names))
           (ottn-exists? (find ottn existing-tables :test #'string=))
           (*db-verbose* nil)
           )
      (unless (find *organisms-table-name* existing-tables :test #'string=)
        (cformatt "*** WHOA!  The ~A table does not exist!  Giving up!"
                 *organisms-table-name*)
        (return-from delete-organism-from-db nil))
      (if ottn-exists?
          (let ((table-names (tables-belonging-to-organism organism-name)))
            ;; TABLE-NAMES contains the name of the tables table.
            (let ((*db-verbose* verbose?))
              (dolist (table-name table-names) 
                (delete-database-table table-name))))
        (progn
          (cformatt "*** No table listing the tables containing data for")
          (cformatt "*** ~A exists.  Table should be called ~A"
                   organism-name ottn)
          (cformatt "*** No tables will be removed from the database.")))
      (let ((*db-verbose* verbose?))
        (esql (formatn 
               "delete from ~A where ~A = '~A'"
               *organisms-table-name* *organisms-table-key* organism-name
               )))
      (when verbose? 
        (cformatt "Organism ~A removed from db." organism-name))
      )))
  
(defun delete-organism-seqinfo (orgf &key (verbose? t))
  (let* ((orgn (lcstring orgf))
         (seqfile (seqinfo-seq-file orgn))
         (seqidxfile (seqinfo-index-file orgn)))
    (when verbose? (cformatt "Deleting file ~A" seqfile))
    (delete-file seqfile)
    (when verbose? (cformatt "Deleting file ~A" seqidxfile))    
    (delete-file seqidxfile)
    (warn "DATA DIRECTORY, ~A, FOR ORGANISM ~A NOT REMOVED."
          (seqinfo-directory orgn) orgf)))


(defun delete-organism (organism &key (verbose? t))
  (delete-organism-from-db organism :verbose? verbose?)
  (delete-organism-seqinfo organism :verbose? verbose?))
      

;; Make sure a given table doesn't already exist, or if it does
;; and DROP? is true, delete it.
(defun verify-table-nonexistence (table-names table-name drop?)
  (when (member table-name table-names :test #'string=)
    (if drop?
        (progn
          (cformatt "Deleting existing table ~A" table-name)
          (delete-database-table table-name))
      (error "A table named ~A already exists in the organisms database."
             table-name
             ))))

;; Make sure a given table is present in a list of tables, or if not
;; and CREATE-FUNCTION is non-nil, call the CREATE-FUNCTION with its
;; CREATE-ARGS to create the table.
(defun verify-table-existence 
       (table-names table-name &optional create-function &rest create-args)
  (when (not (member table-name table-names :test #'string=))
    (if create-function
        (progn
          (cformatt "Creating new table named ~A" table-name)
          (apply create-function create-args))
      (error "No table named ~A exists in the organisms database!"
             table-name
             ))))

(defun organism-element-prefix (element)
  (if (eq element :organism)
      (or (assocadr :organism-prefix *organism-plist*) "")
    (let ((element-property 
           (keywordize (one-string (string element) "-PREFIX"))))
      (or (assocadr element-property *organism-plist*)
          (assocadr :organism-prefix *organism-plist*)
          ""
          ))))


(defun add-organism-element-prefix-to-keys (keys prefix)
  (loop for key in keys collect (one-string prefix key)))
