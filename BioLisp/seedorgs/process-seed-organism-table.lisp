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

(defparameter *seed-organism-table-row-index* 0)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *seed-organism-table-column-info*
    '(
      (:gid nil)
      (:domain nil)
      (:real-domain nil)
      (:full-name (lambda (x) (substitute #\- #\Space x)))
      (:organism-prefix (lambda (x) (substitute #\- #\Space x)))
      (:org-nicknames (lambda (x) (process-seed-master-table-nicknames-list x)))
      (:gene-pattern (lambda (x) (null-field->nil x)))
      (:alt-gene-pattern (lambda (x) (null-field->nil x)))
      (:gene-prefix (lambda (x) (null-field->nil x)))
      (:provisional 
       (lambda (x) (process-seed-master-table-provisional-field x)))
      (:circular (lambda (x) (process-seed-master-table-circular-field x)))
      #+add-complete
      (:complete (lambda (x) (process-seed-master-table-complete-field x)))
      (:host (lambda (x) (null-field->nil x)))
      (:systematic-name (lambda (x) (null-field->nil x)))
      (:lifestyle (lambda (x) (null-field->nil x)))
      (:phylogeny (lambda (x) (null-field->nil x)))
      (:taxonomy-id (lambda (x) (null-field->nil x)))
      (:contigs (lambda (x) (null-field->nil x)))
      (:number-of-contigs (lambda (x) (null-field->nil x)))
      (:total-length 
       (lambda (x) (process-seed-master-table-total-length-field x)))
      (:gc% (lambda (x) (process-seed-master-table-gc%-field x)))
      (:seed? (lambda (x) (null-field->nil x)))
      ))
  )

(defmacro seed-organism-table-accessors ()
  `(progn
     (defvar *seed-organism-table-ncolumns* 
       ,(length *seed-organism-table-column-info*))
     ,@(loop 
        for (keyword function) in *seed-organism-table-column-info*
        for index from 0
        as accessor = (string-upcase (s+ "m-" (string keyword) "-slot"))
        as setter = (string-upcase (s+ "set-" accessor))
        as processor = (string-upcase (s+ "process-" accessor))
        collect
        `(progn
           (defun ,(intern accessor :bio) (record)
             (nth ,index record))
           (defun ,(intern setter :bio) (record value)
             (setf (nth ,index record) value))
           (defun ,(intern processor :bio) (x) 
             ,(if function `(funcall ,function x) 'x)
             )))))

(seed-organism-table-accessors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-master-seed-organism-list (&key (force nil))
  (when (or force (null *external-seed-master-hash*))
    (let ((elhai-table-data nil)
          (processed-elhai-table-data nil)
          (elhai-table-valid-records nil)
          )
    (setq elhai-table-data 
          (read-elhai-gid-table *seed-gid-table-path*))
    (setq processed-elhai-table-data
          (elhai-data->seed-master-table elhai-table-data))
    (setq elhai-table-valid-records 
          (verify-elhai-gid-table-uniqueness processed-elhai-table-data))
    (setq *external-seed-master-hash* 
          (turn-master-list-data-into-hash elhai-table-valid-records))
    (verify-gid-table-constraints)
    (compare-previous-master-list-gids)
    )))

(defun read-elhai-gid-table (file)
  (vformatt "Reading Elhai master seed info table from ~A" file)
  (let* ((filedata 
          (remove-master-list-comment-lines (file-to-string-list file)))
         (records (clean-up-gid-table-records filedata))
         (more-count 0))
    ;; get rid of spurious tab-induced element at end of master table records
    (setq 
     records 
     (loop for record in records
           for linecount from 1
           as rlen = (length record)
           with mtn = *seed-organism-table-ncolumns*
           collect
           (cond
            ((= rlen mtn) record)
            ((> rlen mtn) 
             (incf more-count)
             (subseq record 0 mtn))
            ((< rlen mtn) 
             (vformatt 
              (one-string-nl
               "*** Master table record on line ~D only has ~D items;" 
               "*** It should have at least ~D items.")
              linecount rlen mtn
              )
             (append record (make-list (- mtn rlen) :initial-element nil))
             ))))
    (when (plusp more-count)
      (vformatt "*** ~D master table records have more than ~D columns!"
                more-count *seed-organism-table-ncolumns*))
    (vformatt "  ~D records read..."  (- (length records) 1))
    ;; Could verify headers against keywords above
    (let ((headers (first records))
          (data (cdr records)))
      (verify-master-list-headers-against-keywords headers)
      data
      )))

(defun elhai-data->seed-master-table (data)
  (setq data (verify-required-master-table-fields data))
  (let ((*seed-organism-table-row-index* 1))
    (mapcar 
     (lambda (record) 
       (incf *seed-organism-table-row-index*)
       ;; Transform each item in the master list record if appropriate
       ;; Only certain items need to be processed; those not processed are 
       ;; left as is
       (loop for datum in record 
             for j from 0 
             for (nil function) in *seed-organism-table-column-info*
             collect 
             (if function (funcall function datum) datum)
             ))
     data
     )))

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
          (dups table 'm-organism-prefix-slot))

      ;; make sure no prefixes have internal '.'s 
      (loop for record in table 
            for pos from 0
            as prefix = (m-organism-prefix-slot record)
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
       as nicknames = (copy-list (m-org-nicknames-slot record))
       as internal-non-dups = (purge-duplicates nicknames :test 'string-equal)
       do
       (when (/= (length nicknames) (length internal-non-dups))
         (push (list pos (copy-tree record)) alias-internal-dup-info)
         (set-m-org-nicknames-slot record internal-non-dups)
         ;; (setf (m-nicknames-slot record) internal-non-dups)
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
          (set-m-org-nicknames-slot record internal-non-dups)
          ;; (setf (m-nicknames-slot record) internal-non-dups)
          )))

      ;; Report all GID duplicates
      (report-dups gid-dup-info "GIDs" "GID" 'm-gid-slot)
      ;; Report all Gname duplicates
      (report-dups gname-dup-info "gnames" "gname" 'm-full-name-slot)
      ;; Report all Prefix duplicates
      (report-dups prefix-dup-info "Prefixes" "Prefix" 'm-organism-prefix-slot)

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
             (m-gid-slot data) (m-org-nicknames-slot data) dup-pos
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

(defun turn-master-list-data-into-hash (elhai-data)
  (vformatt "Creating hash table from master list data")
  (let ((elhai-hash (make-string-equal-hash-table)))
    (loop for record in elhai-data
          as key = (m-gid-slot record)
          do
          (setf (gethash key elhai-hash) record)
          )
    elhai-hash
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
    (unless (= (length header-keys) *seed-organism-table-ncolumns*)
      (warn 
       (one-string-nl
        "There are ~D columns in the master table but only ~D columns"
        "are specified in the information data structure.")
       (length header-keys) *seed-organism-table-ncolumns*
       ))
    ;; Check that we know about every column in the master table 
    (loop for key in header-keys 
          for (keyword nil) in *seed-organism-table-column-info*
          for j from 0 
          do
          (unless (eq key keyword)
            (error
             (one-string-nl
              "The information for the master table says that the"
              "header ~A should be the column header for column ~D"
              "but that column header is actually ~A.")
             keyword j key
             )))))
          
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
         (set-m-alt-gene-pattern-slot value nil)
         ;; (setf (m-alt-gene-pattern-slot value) nil)
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

;; For the nonce, every organism must have a gid, a full name, and 
;; an organism-prefix at a minimum.  

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
                 "***  This record will be ignored!"
                 "*** Actual record: ~S")
                record-count org-prefix record
                ))
             (not ok?)
             ))
         ))
     data
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defun process-seed-master-table-circular-field (x)
  (cond
   ((string-equal "T" x) t)
   ((string-equal "NIL" x) nil)
   ((null-master-table-record-field? x)
    (progn
      (vformatt 
       (one-string-nl
        "*** The CIRCULAR field on line ~D of the master table"
        "*** is a null field.  It is supposed to be T or NIL or a list"
        "*** of numbers.")
       *seed-organism-table-row-index*
       )
      nil
      ))
   (t 
    (let ((list (ignore-errors (read-from-string x))))
      (if (and list (listp list) (every 'integerp list))
          list 
        (progn
          (vformatt 
           (one-string-nl
            "*** The CIRCULAR field on line ~D of the master table"
            "*** is neither T, NIL, or a list of integers!"
            "*** It is: ~A")
           *seed-organism-table-row-index* x
           )
          nil
          ))))))

(defun process-t-nil-blank-ml-field (x field)
  (cond
   ((string-equal "T" x) t)
   ((string-equal "NIL" x) nil)
   ((null-master-table-record-field? x) nil)
   (t
    (vformatt 
     (one-string-nl
      "*** The ~A field on line ~D of the master table"
      "*** is not recognizable as either T or NIL.  It is '~A'."
      "*** It will be treated as NIL.")
     field *seed-organism-table-row-index* x
     )
    nil
    )))

(defun process-seed-master-table-complete-field (x)
  (process-t-nil-blank-ml-field x :complete))

(defun process-seed-master-table-provisional-field (x)
  (process-t-nil-blank-ml-field x :provisional))

(defun process-seed-master-table-total-length-field (x)
  (if (null-master-table-record-field? x)
      nil
    (let ((tlen (ignore-errors (read-from-string x))))
      (if (and (integerp tlen) (> tlen 0))
          tlen
        (progn
          (vformatt
           (one-string-nl
            "*** The Total-length value on line ~D of the master table"
            "*** is not a positive integer.  It is: ~A.")
           *seed-organism-table-row-index* x
           )
          nil
          )))))

(defun process-seed-master-table-gc%-field (x)
  ;; convert to an actual number or flag if unreadable
  (if (null-master-table-record-field? x)
      nil
    (let ((percent (ignore-errors (read-from-string x))))
      (if (numberp percent) 
          (if (and (>= percent 0) (<= percent 100))
              percent 
            (progn
              (vformatt 
               (one-string-nl
                "*** The GC% value on line ~D of the master table is not"
                "*** a percentage (a number between 0 and 100).")
               *seed-organism-table-row-index*
               )
              nil
              ))
        (progn
          (vformatt 
           (one-string-nl
            "*** The GC% on line ~D of the master table should be"
            "*** numeric but is not.  It is: ~A.")
           *seed-organism-table-row-index* x
           )
          nil
          )))))

(defun null-field->nil (x) 
  (if (null-master-table-record-field? x) nil x)
  )
           
(defun null-master-table-record-field? (x)
  (or (null x) (every 'whitespacep x) (string= x "-")))

    
    

