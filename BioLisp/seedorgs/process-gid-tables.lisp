;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar
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


(defun gmtrv (record key)
  (nth (cdr (assoc key *master-table-column-indices*)) record))

(defun m-gid-slot (x) (gmtrv x :gid))
(defun m-full-name-slot (x) (gmtrv x :full-name))
(defun m-domain-slot (x) (gmtrv x :domain))
(defun m-real-domain-slot (x) (gmtrv x :real-domain))
(defun m-prefix-slot (x) (gmtrv x :organism-prefix))
(defun m-organism-prefix-slot (x) (gmtrv x :organism-prefix))
(defun m-nicknames-slot (x) (gmtrv x :org-nicknames))
(defun m-org-nicknames-slot (x) (gmtrv x :org-nicknames))
(defun m-gene-pattern-slot (x) (gmtrv x :gene-pattern))
(defun m-alt-gene-pattern-slot (x) (gmtrv x :alt-gene-pattern))
(defun m-gene-prefix-slot (x) (gmtrv x :gene-prefix))

(defsetf m-nicknames-slot set-m-nicknames-slot)
(defsetf m-alt-gene-pattern-slot set-m-alt-gene-pattern-slot)

(defun set-m-nicknames-slot (x v) 
  (setf (nth (cdr (assoc :org-nicknames *master-table-column-indices*)) x) v))

(defun set-m-alt-gene-pattern-slot (x v)
  (setf 
   (nth (cdr (assoc :alt-gene-pattern *master-table-column-indices*)) x)
   v
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-master-seed-organism-list (&key (force nil))
  (when (or force (null *external-seed-master-hash*))
    (let ((elhai-table-data nil)
          (ramy-table-data nil)
          (processed-elhai-table-data nil)
          (processed-ramy-table-data nil)
          (elhai-table-valid-records nil)
          (ramy-table-valid-records nil)
          )
    (setq elhai-table-data 
          (read-elhai-gid-table *seed-gid-table-path*))
    (setq ramy-table-data
          (when *use-new-ramy-table*
            (read-ramy-gid-table *seed-ramy-table-path*)
            ))
    (setq processed-elhai-table-data
          (elhai-data->seed-master-table elhai-table-data))
    (setq processed-ramy-table-data
          (when *use-new-ramy-table*
            (ramy-data->seed-master-table ramy-table-data)
            ))
    (setq elhai-table-valid-records 
          (verify-elhai-gid-table-uniqueness processed-elhai-table-data))
    (setq ramy-table-valid-records
          (when *use-new-ramy-table*
            (verify-ramy-table-uniqueness processed-ramy-table-data)
            ))
    (setq *external-seed-master-hash*
          (merge-elhai-and-ramy-records
           elhai-table-valid-records ramy-table-valid-records
           ))
    (verify-gid-table-constraints)
    (compare-previous-master-list-gids)
    )))

(defun read-elhai-gid-table (file)
  (vformatt "Reading Elhai master seed info table from ~A" file)
  (let* ((filedata 
          (remove-master-list-comment-lines (file-to-string-list file)))
         (records (clean-up-gid-table-records filedata)))
    ;; get rid of spurious tab-induced element at end of master table records
    (setq 
     records 
     (loop for record in records
           for linecount from 2 
           as rlen = (length record)
           with mtn = *master-table-ncolumns*
           collect
           (cond
            ((= rlen mtn) record)
            ((> rlen mtn) (subseq record 0 mtn))
            ((< rlen mtn) 
             (vformatt 
              (one-string-nl
               "*** Master table record (line ~D) only has ~D items;" 
               "*** It should have ~D items.")
              linecount rlen mtn
              )
             (append record (make-list (- mtn rlen) :initial-element nil))
             ))))
    (vformatt "  ~D records read..."  (- (length records) 1))
    ;; Could verify headers against keywords above
    (let ((headers (first records))
          (data (cdr records)))
      (verify-master-list-headers-against-keywords headers)
      data
      )))

(defun read-ramy-gid-table (file)
  (vformatt "Reading Phantome (ramy) phage table from ~A" file)
  (let* ((filedata (file-to-string-list file))
         (phantome-records (clean-up-gid-table-records filedata))
         (phantome-data-records (cddr phantome-records))
         )
    phantome-data-records
    ))

(defun elhai-data->seed-master-table (data)
  (setq data (verify-required-master-table-fields data))
  (flet ((null-field->nil (x) 
           (if (null-master-table-record-field? x) nil x)
           ))
    (mapcar 
     (lambda (record) 
       ;; Transform each item in the master list record if appropriate
       ;; Only certain items need to be processed; those not processed are 
       ;; left as is
       (loop for datum in record for j from 0 
             as key = (car (rassoc j *master-table-column-indices*))
             collect
             (case key 
               (:full-name 
                (substitute #\- #\Space datum))
               (:org-prefix 
                (substitute #\- #\Space datum))
               (:org-nicknames 
                (process-seed-master-table-nicknames-list datum))
               (:gene-pattern
                (null-field->nil datum))
               (:alt-gene-pattern 
                (null-field->nil datum))
               (:gene-prefix
                (null-field->nil datum))
               (otherwise datum)
               )))
     data
     )))  

(defun ramy-data->seed-master-table (data)
  ;; First combine some of the data fields in the ramy table 
  ;; (e.g., fields 4-8 get merged into a single field)
  (vformatt "Processing the ramy data...")
  (setq 
   data 
   (remove-if 
    'null
    (loop for record in data 
          for count from 3
          collect
          (multiple-value-bind (internal-record ok?)
              (ramy-table-record->ramy-internal-record record)
            (if ok?
                internal-record
              (progn
                (vformatt
                 (one-string-nl
                  "Ramy record ~S at line ~D of length ~D is not long enough!"
                  "It should contain at least 24 data elements."
                  "This record will be ignored.")
                 record count (length record))
                nil
                ))))))
  ;; now potentially verify the data in each field and coerce it
  ;; if necessary (e.g., "" -> nil, or a number string to a number)
  (let* ((headers (cons :gid *ramy-slot-keywords*))
         (record-size (length headers)))
    (remove-if 
     'null 
     (loop for record in data
           for count from 3
           collect
           (flet ((null-field->nil (x) 
                    (if (or (null x) (every 'whitespacep x)) nil x)))
             (if (/= (length record) record-size)
                 (progn
                   (vformatt 
                    (one-string-nl
                     "Ramy record ~S, length ~D not processed into ~D slots!"
                     "This record (line ~D of ramy file) will be ignored.")
                    record (length record) record-size count
                    )
                   nil
                   )
               (loop for datum in record
                     for keyword in headers 
                     collect
                     (ecase keyword
                       (:gid datum)
                       (:host-phylogeny datum)
                       (:host datum)
                       (:syst-name (null-field->nil datum))
                       (:lifestyle (null-field->nil datum))
                       (:virus-phylogeny datum)
                       (:taxid (null-field->nil datum))
                       (:contigs (null-field->nil datum))
                       (:n-contigs (null-field->nil datum))
                       (:total-length (null-field->nil datum))
                       (:gc% (null-field->nil datum))
                       (:seed? (null-field->nil datum))
                       (:nucleic-acid (null-field->nil datum))
                       ))))))))
  
(defun ramy-table-record->ramy-internal-record (record)
  (if (>= (length record) 24)
      (values 
       (append 
        ;; gid 
        (subseq record 0 1)
        ;; host phylogeny
        (list (subseq record 4 9))   
        ;; host, Sys name, lifestyle
        (subseq record 10 13)        
        ;; virus phylogeny
        (list (subseq record 13 16)) 
        ;; taxID
        (subseq record 17 18)
        ;; n contigs, total length (skip ncbi ids, aka contigs)
        (subseq record 19 21)        
        ;; GC%, SEED?
        (subseq record 22 24)
        ;; nucleic-acid
        (list
         (LET ((NA-string (first (subseq record 13 14))))
           (IF (AND (STRINGP NA-string) (> (LENGTH NA-string) 4))
               (subseq NA-string 0 5)
             ""))) 
        )
       t
       )
    (values record nil)
    ))

 
(defun verify-elhai-gid-table-uniqueness (data)
  
  (vformatt "Checking elhai table for various duplicates...")

  (let ((table data)
        (gid-dup-info nil)
        (gid-dup-pos nil)
        (gname-dup-info nil)
        (gname-dup-pos nil)
        (prefix-dup-info nil)
        (prefix-dup-pos nil)
        (alias-external-dup-info nil)
        (alias-internal-dup-info nil)
        (gene-prefix-dup-info nil)
        (gene-prefix-dup-pos nil)
        (bad-prefix-info nil)
        (bad-prefix-pos nil)
        )

    (declare (ignore gene-prefix-dup-info gene-prefix-dup-pos))

    (flet ((dups (data accessor)
             (loop for record in data
                   with hash = (make-string-equal-hash-table)
                   with dup-info = nil
                   with positions = nil
                   for pos from 0
                   as datum = (funcall accessor record)
                   do
                   (let ((original-pos (gethash datum hash)))
                     (if original-pos
                         (progn
                           (push (list pos original-pos record) dup-info)
                           (push pos positions)
                           )
                       (setf (gethash datum hash) pos)
                       ))
                   finally (return (values dup-info positions))
                   ))
           (remove-dups (data positions-to-remove)
             (loop for record in data
                   for pos from 0
                   when (not (member pos positions-to-remove))
                   collect record
                   ))
           (report-dups (dup-info plural-type type accessor)
             (loop 
              for (dup-pos original-pos dup-record) in dup-info
              for j from 0
              do
              (when (zerop j)
                (cformatt "Duplicate organism ~A found!" plural-type)
                (cformatt "All organism ~A must be unique!~%" plural-type))
              (if (< j 10)
                  (cformatt 
                   (one-string-nl
                    "Duplicate ~A ~A in external seed master GID table!"
                    "  At record pos. ~D, conflicting with record at pos. ~D."
                    "  Information for this organism will be ignored!")
                   type (funcall accessor dup-record) dup-pos original-pos
                   )
                (progn
                  (cformatt 
                   "...and ~D more duplicates!" (- (length dup-info) 10))
                  (return nil)
                  )))))

      ;; Find positions of all records with duplicate GID's and 
      ;; save duplicate records.
      (multiple-value-setq (gid-dup-info gid-dup-pos) 
          (dups table 'm-gid-slot))
      (multiple-value-setq (gname-dup-info gname-dup-pos) 
          (dups table 'm-full-name-slot))
      (multiple-value-setq (prefix-dup-info prefix-dup-pos) 
          (dups table 'm-prefix-slot))

      ;; make sure no prefixes have internal '.'s 
      (loop for record in table 
            for pos from 0
            as prefix = (m-prefix-slot record)
            when prefix 
            do 
            (let ((pos (position #\. prefix)))
              (unless (or (null pos) (= pos (1- (length prefix))))
                (push (list pos record) bad-prefix-info)
                (push pos bad-prefix-pos)
                )))

      ;; Deal with nicknames.  Blarf.
      ;; Check for internal duplicates as well as the normal
      ;; external duplicates.

      (loop 
       with hash = (make-string-equal-hash-table)
       for record in table
       for pos from 0
       as nicknames = (copy-list (m-nicknames-slot record))
       as internal-non-dups = (purge-duplicates nicknames :test 'string-equal)
       do
       (when (/= (length nicknames) (length internal-non-dups))
         (push (list pos (copy-tree record)) alias-internal-dup-info)
         (setf (m-nicknames-slot record) internal-non-dups)
         )
       ;; for each nickname which is not an internal duplicate
       ;; check whether it is a duplicate of an already processed nickname
       (loop 
        for nickname in internal-non-dups
        with external-dups  = nil
        as original-pos = (gethash nickname hash)
        do
        (if original-pos
            (progn
              (push 
               (list pos original-pos nickname (copy-tree record))
               alias-external-dup-info)
              (push nickname external-dups))
          (setf (gethash nickname hash) pos)
          )
        finally
        ;; remove nicknames that are duplicates from this record's
        ;; list of duplicates
        (progn
          (loop for edup in external-dups do
                (setq 
                 internal-non-dups
                 (delete edup internal-non-dups :test 'string-equal)
                 ))
          (setf (m-nicknames-slot record) internal-non-dups)
          )))

      ;; Report all GID duplicates
      (report-dups gid-dup-info "GIDs" "GID" 'm-gid-slot)
      ;; Report all Gname duplicates
      (report-dups gname-dup-info "gnames" "gname" 'm-full-name-slot)
      ;; Report all Prefix duplicates
      (report-dups prefix-dup-info "Prefixes" "Prefix" 'm-prefix-slot)

      ;; Report all external nickname conflicts
      (loop for (dup-pos original-pos nickname data) in alias-external-dup-info
            for j from 0
            do
            (when (zerop j)
              (cformatt 
               "Duplicate nicknames found in external Seed master GID table!")
              (cformatt "All nicknames across all organisms must be unique!~%"))
            (if (< j 10)
                (cformatt 
                 (one-string-nl
                  "Duplicate nickname ~A found associated with GID ~A !"
                  "  Conflicts with nickname given to GID ~A."
                  "  Duplicate record is at pos. ~D, conflicting with pos. ~D")
                 nickname (m-gid-slot data) 
                 (m-gid-slot (nth original-pos table))
                 dup-pos original-pos)
              (progn
                (cformatt
                 "...and ~D more duplicates!" 
                 (- (length alias-external-dup-info) 10))
                (return nil)
                )))

      ;; report internal nickname conflicts
      (when alias-internal-dup-info 
        (cformatt 
         (one-string-nl
          "Some duplicated nicknames within organisms found"
          "in external seed master gid table!"
          )))
      (loop for (dup-pos data) in alias-internal-dup-info
            do
            (cformatt 
             (one-string-nl
              "Duplicated nicknames found in nicknames list for GID ~A:"
              "  Nicknames list: ~A "
              "  GID at position ~D in Seed GID table.")
             (m-gid-slot data) (m-nicknames-slot data) dup-pos
             ))

      ;; report bad prefixes
      (loop for (bad-pos record) in bad-prefix-info
            do
            (cformatt
             (one-string-nl
              "No '.' allowed anywhere in any prefix except at end!"
              "  Organism ~A, prefix: ~S at position ~D in Seed gid table!"
              "  Information for this organism will be ignored!")
             (m-gid-slot record) (m-organism-prefix-slot record) bad-pos
             ))

      ;; get rid of all records with duplicates or other problems
      (setq 
       table
       (remove-dups 
        table
        (union 
         (union
          (union gid-dup-pos gname-dup-pos) 
          prefix-dup-pos)
         bad-prefix-pos
         )))
      
      table

      )))

(defun verify-ramy-table-uniqueness (data)
  ;; check for and ignore any records with duplicate gids
  (vformatt "Checking for ramy table duplicates...")
  (let ((duphash (make-hash-table)))
    (remove-if 
     'null
     (loop for record in data 
           for count from 3
           as gid = (first record)
           collect
           (if (gethash gid duphash)
               (progn
                 (vformatt 
                  (one-string-nl
                   "Duplicate gid ~A near line ~D found in ramy table!"
                   "This record will be ignored.")
                  gid count)
                 nil
                 )
             (progn
               (setf (gethash gid duphash) t)
               record
               ))))))

(defun merge-elhai-and-ramy-records (elhai-data ramy-data)
  (vformatt "Merging elhai and ramy data...")
  (let ((elhai-hash (make-string-equal-hash-table))
        (ramy-hash (make-string-equal-hash-table))
        (combined-hash (make-string-equal-hash-table))
        (no-ramy-count 0)
        (merge-count 0)
        (no-elhai-count 0)
        (combined-list nil))
    (loop for record in elhai-data
          as key = (m-gid-slot record)
          do
          (setf (gethash key elhai-hash) record))
    (loop for record in ramy-data 
          as key = (first record)
          do
          (setf (gethash key ramy-hash) record))
    (vformatt "Merging master table data with ramy table data...")
    (maphash
     (lambda (key elhai-record)
       (let ((ramy-record (gethash key ramy-hash)))
         (cond
          (ramy-record 
           ;; strip off the gid from the ramy record and append the
           ;; rest of its data to the elhai data
           (let ((merged-record (append elhai-record (cdr ramy-record))))
             (push merged-record combined-list))
           (incf merge-count))
          (t 
           (incf no-ramy-count)
           (push elhai-record combined-list)
           ))))
     elhai-hash
     )
    (maphash 
     (lambda (key ramy-record)
       (declare (ignore ramy-record))
       (let ((elhai-record (gethash key elhai-hash)))
         (cond
          (elhai-record nil)
          (t
           (incf no-elhai-count)
           (vformatt "~A : No elhai table record for this Ramy gid" key)
           ))))
     ramy-hash
     )
    (vformatt "Merged ~D elhai list and ramy records..." merge-count)
    (vformatt "A total of ~D elhai list gids not in ramy table..." 
              no-ramy-count)
    (when (plusp no-elhai-count) 
      (vformatt 
       "A total of ~D ramy gids exist which are not in the elhai table..."
       no-elhai-count
       ))
    (loop for record in combined-list 
          do
          (setf 
           (gethash (m-gid-slot record) combined-hash)
           record
           ))
    combined-hash
    ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-master-list-comment-lines (lines)
  (remove-if 
   (lambda (line) 
     (or (every 'whitespacep line)
         (and (plusp (length line)) (char= #\; (char line 0)))
         ))
   lines
   ))

(defun verify-master-list-headers-against-keywords (headers)
  (let ((header-keys (mapcar 'keywordize headers)))
    ;; Check that we know about every column in the master table 
    (loop for key in header-keys 
          for j from 0 
          as index = (cdr (assoc key *master-table-column-indices*))
          do
          (cond
           ((null index) 
            (warn
             (one-string-nl
              "The master table has a column titled ~A"
              "but the code is not cognizant of that column heading.")
             key
             ))
           ((/= index j)
            (error 
             (one-string-nl
              "The master table column titled ~A is at column ~D"
              "but the code thinks that it should be in column ~D")
             key j index
             ))
           (t nil)
           ))
    ;; Check that every column the code knows is in fact in the master table
    (loop for (key . nil) in *master-table-column-indices*
          do
          (unless (find key header-keys)
            (funcall 
             (if (member key *not-yet-used-master-table-columns*)
                 'warn
               'error)
             (one-string-nl
              "The code believes there should be a column titled ~A"
              "but the master table has no such column.")
             key
             )))))

(defun null-master-table-record-field? (x)
  (or (null x) (every 'whitespacep x) (string= x "-")))

(defun invalid-master-list-prefix-char (ch)
  (and (not (digit-char-p ch)) 
       (not (alpha-char-p ch))
       (not (char-equal ch #\-))
       (not (char-equal ch #\_))
       ))

(defun process-seed-master-table-nicknames-list (nicknames-list-as-string)
  (if (equalp nicknames-list-as-string "-")
      nil
    (let* ((trimmed (string-trim *whitespace* nicknames-list-as-string))
           (nicknames (subseq trimmed 1 (1- (length trimmed))))
           (nicknames-list 
            (remove-if 
             (lambda (x) (equal x ""))
             (string-split nicknames #\Space)
             )))
      nicknames-list
      )))

;; The only constraint currently is that a master list record
;; can only have one of gene-pattern and alt-gene-pattern, but not both
(defun verify-gid-table-constraints ()
  (vformatt "Verifying constraints on gid table...")
  (maphash 
   (lambda (key value)
     (when (m-gene-pattern-slot value) 
       (when (m-alt-gene-pattern-slot value)
         (cformatt 
          (one-string-nl
           "Master list entry ~A has both a gene pattern ~A"
           "and alt gene pattern ~A!"
           "You may have one or the other but not both!"
           "The alt gene pattern will be ignored!")
          key (m-gene-pattern-slot value) (m-alt-gene-pattern-slot value)
          )
         (setf (m-alt-gene-pattern-slot value) nil)
         )))
   *external-seed-master-hash*
   ))

(defun compare-previous-master-list-gids ()
  (let* ((previous-gids (slotv #$master-list-gids #$list))
         (gids 
          (lmaphash
           (lambda (key value) (declare (ignore value)) key)
           *external-seed-master-hash*
           ))
         (deleted-gids (set-difference previous-gids gids :test 'string-equal))
         (added-gids (set-difference gids previous-gids :test 'string-equal)))
    (when added-gids
      (cformatt 
       "~D gids were added to the master table since previous boot!" 
       (length added-gids))
      (report-added-gids added-gids))
    (when deleted-gids
      (cformatt 
       "~D gids were deleted from the master table since previous boot!" 
       (length deleted-gids))
      (report-deleted-gids deleted-gids))
    (cformatt "Storing master list gids for posterity...")
    (setf (slotv #$master-list-gids #$list) gids)
    nil
    ))

(defun verify-required-master-table-fields (data)
  (let ((record-count 0))
    (remove-if 
     (lambda (record)
       (incf record-count)
       (let ((gid (m-gid-slot record))
             (full-name (m-full-name-slot record))
             (org-prefix (m-organism-prefix-slot record))
             (ok? t))
         (when (null-master-table-record-field? gid)
           (setq ok? nil)
           (cformatt 
            (one-string-nl
             "*** Record ~D of the master file does not contain a gid!"
             "***  All master file records must contain a non-duplicated"
             "***  gid!  This record will be ignored!"
             "***  Actual record: ~S")
            record-count record
            ))
         (when (null-master-table-record-field? full-name)
           (setq ok? nil)
           (cformatt 
            (one-string-nl
             "*** Record ~D of the master file does not contain an organism"
             "***  name!  All master file records must contain a non-duplicated"
             "***  organism name!  This record will be ignored!"
             "***  Actual record: ~S")
            record-count record
            ))
         (if (null-master-table-record-field? org-prefix)
             (progn
               (setq ok? nil)
               (cformatt 
                (one-string-nl
                 "*** Record ~D of the master file does not contain an organism"
                 "***  prefix! All master file records must contain a"
                 "***  non-duplicated organism prefix! "
                 "***  This record will be ignored!"
                 "***  Actual record: ~S")
                record-count record
                )
               (not ok?))
           (progn
             (when (some 'invalid-master-list-prefix-char org-prefix)
               (setq ok? nil)
               (cformatt
                (one-string-nl
                 "*** Record ~D of the master file contains an invalid organism"
                 "*** prefix!  Prefix is ~S.  But prefixes can only contain"
                 "*** digits, alphabetic characters, and dashes."
                 "*** Actual record: ~S")
                record-count org-prefix record
                ))
             (not ok?)
             ))
         ))
     data
     )))


    
    

