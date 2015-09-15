;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar, Jeff Elhai.  

(defparameter *ncbi-base-contig-info-url*
  (one-string
   "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   "efetch.fcgi?db=nucleotide&rettype=gb&id="))

(defun read-contig-info-complete-file (orgfile)
  (when (probe-file orgfile)
    (let* ((data (file-to-string-list orgfile)))
      (mapcar 'read-from-string data)
      )))

(defun add-records-to-text-file (orgfile records &key (blank-line? nil))
  (with-open-file 
      (p orgfile 
         :direction :output :if-exists :append :if-does-not-exist :create)
    (loop for record in records do (format p "~S~%" record))
    (when blank-line? (terpri p))
    ))

(defun add-orgf-to-contig-info-complete-file (orgfile orgf)
  (add-records-to-text-file orgfile (list orgf)))

(defun add-contig-info-to-complete-contigs-file (file retrieved-contigs)
  (add-records-to-text-file file retrieved-contigs))

(defun add-contig-info-to-partial-contigs-file (file partial-contigs)
  (add-records-to-text-file file partial-contigs :blank-line? t))
  

;; This function extracts information for the contigs of organisms
;; from ncbi.  

;; Information is kept in two files: 
;; .../Organisms/ncbi-contig-info-status.lisp
;; .../Organisms/ncbi-contig-info.lisp 
;;
;; The contig info status file contains information about which
;; organisms have already had their data extracted from ncbi, and the status
;; of that data.  There is one list per organism.  
;; 
;; Each list is of the form (orgf type status)
;;   where type is either :all (info for all contigs was to be retrieved)
;;     or :first-few (info for just the first 3 contigs was to be 
;;     retrieved because there are more than 10 of them).
;;   where status is either :ok (info for all contigs was successfully
;;     retrieved)
;;     or :no-matches (no info for any contig was retrieved)
;;     or a list of statuses for each contig (info for some of the contigs
;;     was retrieved and no info for some other contigs was found).  
;;
;; The contig info file contains the extracted information about each
;; contig whose information was successfully extracted from ncbi.  
;; There is one list per contig.  
;; 
;; Each list is of the form (orgf contig-frame extracted-info)
;; where extracted-info is of the form 
;; (contig-id plasmid? complete? circular-or-linear-string description-string)

(defun extract-contig-info-from-ncbi
       (&key 
        (max 100) (n-if-incomplete 3) 
        (orgs-to-retrieve (bio::available-organisms))
        (org-info-file 
         (utils::webpath->lisp-physical-pathname 
          "biol:Organisms;ncbi-contig-info-status.lisp"))
        (contig-info-file
         (utils::webpath->lisp-physical-pathname 
          "biol:Organisms;ncbi-contig-info.lisp")))
  (let* ((orginfo (read-ncbi-orginfo-file org-info-file))
         (orgframes (mapcar 'first orginfo))
         (available-orgs orgs-to-retrieve)
         (orgs-not-yet-obtained (set-difference available-orgs orgframes))
         )
    (formatt 
     "~%;; ~D organisms have not had their contig information retrieved yet!~%"
     (length orgs-not-yet-obtained))
    (cformatt "Retrieving contig information for at most ~D organisms." max)
    (loop 
     with ok-list = nil
     with no-matches-list = nil
     with orgs-attempted-list = nil
     for orgf in orgs-not-yet-obtained 
     for n from 0 below max
     as contigs = (#^contiguous-sequences orgf)
     as ncontigs = (length contigs)
     as type = (if (<= ncontigs 10) :all :first-few)
     as status-list = nil
     do
     (print orgf)
     (loop 
      for contig in contigs 
      for count from 1 to 
      (case type 
        (:all  most-positive-fixnum)
        (:first-few n-if-incomplete))
      do
      (multiple-value-bind (record status)
          (get-contig-info-from-ncbi-improved contig)
        (push (list contig status record) status-list)
        ))
     (setq status-list (nreverse status-list))
     (cond
      ;; every contig's info was retrieved.  add a record
      ;; to the orginfo file that this organism's contigs 
      ;; were all retrieved ok, and records for each contig 
      ;; to the contig info file with the ncbi info.  
      ((every (lambda (x) (eq :ok (second x))) status-list)
       (loop for (contig nil record) in status-list
             do 
             (add-contig-record-to-ncbi-contig-info-file 
              contig-info-file (list orgf contig record)
              ))
       (add-orgf-record-to-ncbi-orginfo-file
        org-info-file (list orgf type :ok))
       (push orgf ok-list)
       )
      ;; none of the contigs for this organism were found in ncbi.
      ;; add a record to the orginfo file noting this.  
      ((every (lambda (x) (eq :no-match (second x))) status-list) 
       (add-orgf-record-to-ncbi-orginfo-file
        org-info-file (list orgf type :no-matches))
       (push orgf no-matches-list)
       )
      ;; some of the contigs for this organism were found in ncbi,
      ;; and some weren't.  Create records in the contig info file
      ;; for those that were.  Write a record for the organism,
      ;; noting for each contig whether we got the info or what 
      ;; the problem was.  
      (t 
       (loop for (contig status record) in status-list
             do 
             (when (eq status :ok)
               (add-contig-record-to-ncbi-contig-info-file 
                contig-info-file (list orgf contig record)
                )))
       (add-orgf-record-to-ncbi-orginfo-file
        org-info-file 
        (list orgf type 
              (mapcar
               (lambda (status-record)
                 (list (first status-record) (second status-record)))
               status-list
               )))))
     (push orgf orgs-attempted-list)
     finally
     (progn
       (formatt "~%~D organisms attempted.~%" (length orgs-attempted-list))
       (formatt "~D organisms had all contigs retrieved.~%" (length ok-list))
       (formatt "~D organisms had every contig fail to match.~%"
                (length no-matches-list)
                )))))

;;; Extract contig info for the organisms provided and update
;;; the ncbi_contig_info table on the SEED's MySQL database with
;;; that information.

(defun extract-contig-info-from-ncbi-and-update
       (orgs-to-retrieve
        &key 
        (problem-report-file nil)
        (too-many-contigs-threshold 11)
        (verbose? t)
        &aux
        (n-orgs (length orgs-to-retrieve))
        ok-list no-matches-list partial-success-list too-many-contigs-list
        )
  (when verbose?
    (cformatt "Retrieving contig information for ~D organisms." n-orgs))
  (loop 
   for orgf in orgs-to-retrieve
   for n from 0 
   as contigs = (#^contiguous-sequences orgf)
   as ncontigs = (length contigs)
   as type = (if (or (null too-many-contigs-threshold)
                     (< ncontigs too-many-contigs-threshold))
                 :all
               :too-many)
   as status-list = nil
   do
   (when verbose?
     (cformatt "Retrieving contig info for organism ~A" orgf))
   (cond
    ((eq type :too-many)
     (push orgf too-many-contigs-list))
    ((eq type :all)
     (loop 
      for contig in contigs 
      for count from 1 to ncontigs
      do
      (multiple-value-bind (record status)
          (get-contig-info-from-ncbi-improved contig)
        (push (list contig status record) status-list)
        ))))
   (setq status-list (nreverse status-list))
   (when status-list
     (cond
      ;; every contig's info was retrieved.  
      ;; store this fact and the contig info that was retrieved
      ((every (lambda (x) (eq :ok (second x))) status-list)
       (push (list orgf status-list) ok-list)
       )
      ;; none of the contigs for this organism were found in ncbi.
      ;; note this
      ((every (lambda (x) (eq :no-match (second x))) status-list) 
       (push orgf no-matches-list)
       )
      ;; some of the contigs for this organism were found in ncbi,
      ;; and some weren't.  Create records in the contig info file
      ;; for those that were.  Write a record for the organism,
      ;; noting for each contig whether we got the info or what 
      ;; the problem was.  
      (t 
       (push (list orgf status-list) partial-success-list)
       )))
   finally
   (when verbose?
     (formatt "~%~D organisms attempted.~%" n-orgs)
     (formatt "~D organisms had all contigs retrieved.~%" (length ok-list))
     (formatt "~D organisms had every contig fail to match.~%"
              (length no-matches-list))
     (formatt "~D organisms had some contigs retrieved and others failed.~%"
              (length partial-success-list))
     (when too-many-contigs-threshold
       (formatt "~D organisms had too many contigs (> ~D) to retrieve.~%~%"
                (length too-many-contigs-list) too-many-contigs-threshold
                ))))

  (if (or no-matches-list partial-success-list too-many-contigs-list)
      (if problem-report-file 
          (progn
            (when verbose? 
              (cformatt "Problem report generated to ~A" problem-report-file))
            (with-open-file 
                (p problem-report-file :direction :output :if-exists :supersede)
              (generate-ncbi-problem-report 
               p no-matches-list partial-success-list too-many-contigs-list
               )))
        (generate-ncbi-problem-report 
         t no-matches-list partial-success-list too-many-contigs-list
         ))
    (with-open-file
        (p problem-report-file :direction :output :if-exists :supersede)
      (format p "No problematic organisms!~%")
      ))
  (loop for ok-org-data in ok-list 
        do
        (when verbose? 
          (cformatt 
           "Updating ncbi_contig_info for organism ~A" 
           (first ok-org-data)
           ))
        (update-ncbi-contig-info-table ok-org-data verbose?)
        ))

(defun generate-ncbi-problem-report 
       (p no-matches-list partial-success-list too-many-contigs-list)
  (when no-matches-list 
    (format p "Organisms that had no contigs found at NCBI: ~%")
    (loop for org in no-matches-list do (format p "  ~A~%" org))
    (terpri p))
  (when too-many-contigs-list
    (format p "Organisms that had more contigs than the limit specified:~%")
    (loop for org in too-many-contigs-list do (format p "  ~A~%" org))
    (terpri p))
  (when partial-success-list
    (format 
     p 
     (one-string
      "Organisms of which some contig info was retrieved successfully"
      " and some was not:~%"
      ))
    (loop for (org status-list) in partial-success-list 
          do
          (format p "  ~A~%" org) 
          (loop 
           for (contig status nil) in status-list do
           (cond
            ((eq status :ok) 
             (format p "    Contig ~A: ok~%" contig))
            ((eq status :no-match)
             (format p "    Contig ~A: info not found (no match).~%" contig))
            ((and (listp status) (eq (first status) :error))
             (format p "    Contig ~A: error: ~A~%" contig (second status)))
            (t 
             (format
              p "    Contig ~A: Oops! Unknown status: ~A~%" contig status)
             )))
          (terpri p)
          )))

(defun update-ncbi-contig-info-table (ok-org-data verbose?)
  (declare (ignorable verbose?))
  (flet ((existing-info (gid)
           (bbi-seed-query 
            "select * from ncbi_contig_info where genome = '~A'" gid
            )))
    (let* ((orgf (first ok-org-data))
           (genome-seed-id (#^seed-id orgf))
           (contigs-info (second ok-org-data))
           (ncontigs (length contigs-info))
           )
      (when verbose?
        (let ((n-existing (length (existing-info genome-seed-id))))
          (cformatt "Updating ncbi_contig_info table with ~D records for ~A"
                    ncontigs orgf)
          (when (plusp n-existing)
            (cformatt "  (~D records currently exist...)" n-existing)
            )))
      ;; get rid of the information for all the contigs of this organism 
      ;; in the ncbi_contig_info table
      (when verbose? (cformatt "  Deleting existing information...~%"))
      (delete-genome-ncbi-contig-info genome-seed-id)
      (unless (zerop (length (existing-info genome-seed-id)))
        (error "Delete-query did not remove all ~A (~A) records!" 
               orgf genome-seed-id
               ))
      ;; insert the extracted contig info for each contig one by one
      (loop 
       for (contig-frame nil (ncbi-id plasmid? complete? circular ncbi-name))
       in contigs-info
       as contig-id = (#^seed-id contig-frame)
       as circular? = (string-equal circular "circular")
       as c-ncbi-name = (canonicalize-ncbi-name ncbi-name)
       do
       (insert-ncbi-contig-info
        genome-seed-id contig-id ncbi-id plasmid?
        complete? circular? c-ncbi-name
        ))
      (let ((nrecords (length (existing-info genome-seed-id))))
        (unless (= nrecords ncontigs)
          (error 
           (one-string-nl
            "Inserted ~D records for ~A into ncbi_contig_info table but"
            "there are only ~D records in the table!")
           ncontigs orgf nrecords
           )))
      (when verbose? 
        (cformatt "Update of ncbi_contig_info table for ~A complete" orgf)
        ))))
       


(defun read-ncbi-orginfo-file (orgfile)
  (read-file-as-list-of-forms orgfile))

(defun read-ncbi-contig-info-file (contig-info-file)
  (read-file-as-list-of-forms contig-info-file))

(defun read-file-as-list-of-forms (file)
  (when (probe-file file)
    (with-open-file (p file :direction :input)
      (loop with eof = nil with records = nil 
            until eof 
            as record = (read p nil nil nil)
            do
            (if (null record)
                (setq eof t)
              (push record records))
            finally (return (nreverse records))
            ))))

(defun add-orgf-record-to-ncbi-orginfo-file (orgfile orgf-record)
  (add-records-to-text-file orgfile (list orgf-record)))

(defun add-contig-record-to-ncbi-contig-info-file (contig-file contig-record)
  (add-records-to-text-file contig-file (list contig-record)))

;; This function returns ncbi info about a seed contig.
;; The CONTIG argument could either be a contig frame or a string
;; representing a contig's seed id.  
;; The data is returned in a list.  
;; The first element of the list is the contig id (also the ncbi id).
;; The second element is whether the contig is a plasmid or not (T or NIL).
;; The third element is whether the contig is complete or not (T or NIL).
;; The fourth element is either "linear", "circular" or "segment" 
;;   (anything else?)
;; The fifth element is either some kind of description or the null string
;; or NIL.  

;; If the data returned is NIL, then something went wrong, and the second
;; value gives an indication of the problem.  Possible problems are:
;; The contig id does not represent a unique seed contig.  
;; The contig id does not name a contig found at ncbi.  In this case 
;; the second value is :no-match.  
;; The data extracted from ncbi does not match the format that was expected.
;; In this case the second value is a list whose first element is :error
;; and whose second element is a description of the problem.  

(defun get-contig-info-from-ncbi-improved (contig &key verbose &aux org)
  (block exit
    (case user::*organisms-descriptor*
      (:seed-organisms nil)
      (otherwise (error "Cannot use this function without the seed!")))
    (cond
     ((frames::isframe? contig) (setq org (#^organism contig)))
     ((stringp contig) 
      (multiple-value-bind (cframe orgf msg)
          (forward-funcall 'bio::seed-id-is-contig-id? contig)
        (cond
         (cframe
          (setq org (#^organism cframe))
          (setq contig cframe))
         ((listp orgf) 
          (return-from exit (values nil (list :error "Contig ID not unique!"))))
         (t (return-from exit (values nil (list :error msg))))
         )))
     (t (error "Contig must be a string or a frame!"))
     )
    (LET* ((org-name (SLOTV org #$fname))
           (last-org-word (BBL::LAST (SPLIT org-name EVERY "-")))
           (seed-id (SLOTV contig #$seed-id))
           (ncbi-id (IF (SEARCH "|" seed-id)
                          (BBL::LAST (SPLIT seed-id EVERY "|"))
                          seed-id))
           (base-url *ncbi-base-contig-info-url*)
           (file-contents
            (handler-case
                (bio::web-page-contents (JOIN base-url ncbi-id))
              (error 
               (c)
               (return-from exit 
                 (values
                  nil
                  (list :error (formatn "Problem with webpage-contents: ~A" c))
                  ))))))
      (when (search "Nothing has been found" file-contents :test 'string-equal)
        (return-from exit (values nil :no-match)))
      (let ((headers
              (match-of-pattern NIL "^(LOCUS.*)^ *FEATURES" file-contents '(NIL NIL NIL T))))
        (unless headers
          (return-from exit 
            (values nil (list :error "No match for headers!"))))
        (handler-case
            (let* ((linear-circular
                      (MATCH-OF-PATTERN NIL "LOCUS.*[DR]NA *([^ ]*)"
                               headers '(NIL NIL NIL T)))
                   (definition
                      (MATCH-OF-PATTERN NIL "DEFINITION *(.*?)\\.?.^[^ ]"
                             headers '(NIL NIL NIL T) :CROSS-LINES)))
              (unless definition
                (return-from exit 
                  (values nil (list :error "No match for definition!"))))
              (let* ((is-plasmid?) (complete?)(contig-name)
                     (match-info
                        (MATCH-OF-PATTERN NIL
                               (JOIN  "plasmid[^A-Za-z0-9]*(.*?),\\W*"
                                      ".*?(complete).*$")
                               definition '(NIL NIL NIL T) :CROSS-LINES T))
                    )
                 (IF-TRUE match-info   ; It's a plasmid
                     THEN (SETF is-plasmid? T)
                          (ASSIGN (contig-name complete?) = match-info)
                          (IF (NUMBERP (CONVERT-TO-NUMBER-MAYBE contig-name))
                              (SETF contig-name (JOIN "p" contig-name)))
                     ELSE (SETF match-info
                             (MATCH-OF-PATTERN NIL
                               (JOIN last-org-word "[^A-Za-z]*(.*?),\\W*"
                                     ".*?(complete)?.*?$")
                               definition '(NIL NIL NIL T) :CROSS-LINES T))
                          (IF match-info
                             (ASSIGN (contig-name complete?) = match-info))
                          (IF (AND complete? (EQUAL contig-name ""))
                              (SETF contig-name "chromosome"))
                     )
                 ;; at this point, complete? is either NIL or "complete".
                 ;; convert it to a boolean.  
                 (when (string-equal complete? "complete")
                   (setq complete? t))
                (WHEN verbose
                  (DISPLAY-DATA
                   (ref headers 1 -> 200) definition is-plasmid? match-info))
                (values 
                   (LIST 
                    ncbi-id is-plasmid? complete? linear-circular contig-name)
                   :ok
                 )))
          (error 
           (c)
           (return-from exit 
             (values 
              nil (list :error (formatn "Problem with pattern matches: ~A" c))
              ))))
        ))))


#||

(seed-query "create table ncbi_contig_info (
    genome varchar(16), 
    contig varchar(96),
    is_complete boolean,
    is_plasmid boolean,
    is_circular boolean,
    ncbi_name varchar(96),
    index (genome)
    );
  ")

(seed-query "insert into ncbi_contig_info (genome,contig,is_complete,is_plasmid,is_circular,ncbi_name)
  values
  ('foo','foocontig',true,false,true,'myname');
  ")

(bbi-seed-query "delete from ncbi_contig_info where genome = '~A'" )

||#

(defun bbi-seed-query (query &rest args)
  (apply 'forward-package-funcall :bio :seed-query (cons query args)))

(defun create-ncbi-contig-info-table ()
  (bbi-seed-query "create table ncbi_contig_info (
    genome varchar(16), 
    contig varchar(96),
    ncbi_id varchar(96),       
    is_plasmid boolean,
    is_complete boolean,
    is_circular boolean,
    ncbi_name varchar(96),
    index (genome)
    );
  "))

(defun insert-ncbi-contig-info
       (genome contig ncbi-id plasmid? complete? circular? ncbi-name)
  (flet ((tobool (x) (if x 1 0)))
    (bbi-seed-query 
     (s+
      "insert into ncbi_contig_info "
      "(genome,contig,ncbi_id,is_plasmid,is_complete,is_circular,ncbi_name) "
      "values ("
      (formatn "'~A','~A','~A',~D,~D,~D,'~A');"
               genome contig ncbi-id (tobool plasmid?) (tobool complete?)
               (tobool circular?) ncbi-name
               )))))

(defun delete-genome-ncbi-contig-info (genome-seed-id)
  (bbi-seed-query
   "delete from ncbi_contig_info where genome = '~A'" genome-seed-id)
  )

(defun clean-up-ncbi-name (name)
  (let* ((len (length name))
         (last (1- len))
         (v (make-array (list len))))
    (loop for j from 0 below len
          do
          (setf (aref v j) 
                (and (whitespacep (aref name j))
                     (or (= j last) (whitespacep (aref name (1+ j)))))))
    (let* ((count 0)
           (collapsed-name 
            (remove-if
             (lambda (x) 
               (declare (ignore x))
               (prog1 (aref v count) (incf count))
               )
             name
             )))
      (let ((cclp (char-code #\())
            (ccrp (char-code #\))))
        ;; turn spaces into dashes and remove parentheses
        (replace-chars 
         (substitute #\- #\Space (string-trim *whitespace* collapsed-name))
         `((,cclp "") (,ccrp ""))
         ))
      )))

(defun read-and-store-extracted-ncbi-info 
       (&key
        (org-info-file 
         (utils::webpath->lisp-physical-pathname 
          "biol:Organisms;ncbi-contig-info-status.lisp"))
        (contig-info-file
         (utils::webpath->lisp-physical-pathname 
          "biol:Organisms;ncbi-contig-info.lisp")))
  (let ((tables (mapcar 'first (bbi-seed-query "show tables"))))
    (unless (member "ncbi_contig_info" tables :test 'string-equal)
      (create-ncbi-contig-info-table)
      ))
  (let ((org-status-info (read-ncbi-orginfo-file org-info-file))
        (contig-status-info (read-ncbi-contig-info-file contig-info-file))
        (orgf-hash (make-hash-table))
        (gid-hash (make-hash-table :test 'string-equal))
        ;; get all the gids currently in the ncbi_contig_info seed table.
        (gids 
         (purge-duplicates 
          (mapcar 
           'first
           (bbi-seed-query "select genome from ncbi_contig_info"))
          :test 'string-equal
          )))
    ;; put all the info associated with the contigs of a given organism
    ;; into the organism's hash slot
    (loop for record in contig-status-info 
          as orgf = (first record)
          do
          (push (cdr record) (gethash orgf orgf-hash nil)))
    ;; Store the gids in a hash for fast access
    (loop for gid in gids do (setf (gethash gid gid-hash) t))
    ;; store new contig info into the ncbi_contig_info table
    (loop 
     for (orgf type status) in org-status-info 
     as seed-id = (#^seed-id orgf)
     do
     ;; if this organism's info is already in the ncbi_contig_info table,
     ;; then we don't want to do anything
     (unless (gethash seed-id gid-hash) 
       ;; unless we've extracted information about all this organism's
       ;; contigs, for the moment at least we aren't putting that info
       ;; into the ncbi_contig_info table
       (when (and (eq type :all) (eq status :ok))
         (let ((org-contig-info (gethash orgf orgf-hash)))
           (unless org-contig-info 
             (cformatt 
              (one-string-nl
               "*** Organism ~A in status file but no contig records"
               "found in contig-info-file")
              orgf
              )
             (return)
             )
           (loop 
            for
            (contig-frame (ncbi-id plasmid? complete? circular-or-linear name))
            in org-contig-info 
            as contig-id = (#^seed-id contig-frame)
            as circular? = (string-equal circular-or-linear "circular")
            as ncbi-name = (canonicalize-ncbi-name name)
            do
            (unless (string-equal (#^seed-id contig-frame) ncbi-id)
              (formatt
               (one-string-nl
                "The seed id of contig frame ~A is not what's recorded"
                "in the contig-status-file as that contig's ncbi id, ~A !")
               contig-frame ncbi-id
               )
              )
            (insert-ncbi-contig-info 
             seed-id contig-id ncbi-id plasmid? complete? circular? ncbi-name
             ))))))))

(defun canonicalize-ncbi-name (name)
  (cond
   ((null name) "")
   ((zerop (length name)) name)
   (t (clean-up-ncbi-name name))
   ))
