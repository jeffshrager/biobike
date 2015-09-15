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


(defun gid-slot (x) (first x))
(defun genome-name-slot (x) (second x))
(defun prefix-slot (x) (third x))
(defun nicknames-slot (x) (fourth x))
(defun set-nicknames-slot (x v) (setf (fourth x) v))
(defsetf nicknames-slot set-nicknames-slot)
(defun gene-pattern-slot (x) (fifth x))
(defun alt-gene-pattern-slot (x) (sixth x))
(defun gene-prefix-slot (x) (seventh x))

(defparameter *use-external-gid-table?* t)
(defparameter *use-ramy-gid-table?* t)

(defmacro with-seed-gid-table (&body body)
  `(multiple-value-bind (*gid->gid-table-info* *seed-gid-table*)
       (let* ((elhai-table 
               (when *use-external-gid-table?*
                 (elhai-data->seed-gid-table 
                  (read-elhai-formatted-gid-table *seed-gid-table-path*)
                  )))
              (ramy-table 
               (when *use-ramy-gid-table?*
                 (ramy-data->seed-gid-table 
                  (read-ramy-formatted-gid-table *ramy-phage-table-path*)
                  )))
              (combined-table 
               (combine-elhai-and-ramy-tables elhai-table ramy-table)))
         (values (seed-gid-table->hash combined-table) combined-table)
         )
     ,@body
     ))

(defun read-elhai-formatted-gid-table (file)
  (vformatt "Reading Elhai table from ~A" file)
  (let* ((filedata (file-to-string-list file))
         (records (clean-up-gid-table-records filedata)))
    (vformatt "  ~D records read..."  (- (length records) 1))
    (values (cdr records) (first records))
    ))

(defun read-ramy-formatted-gid-table (file)
  (vformatt "Reading Ramy table from ~A" file)
  (let* ((filedata (file-to-string-list file))
         (records (clean-up-gid-table-records filedata)))
    (vformatt "  ~D records read..." (- (length records) 2))
    (values (cddr records) (second records))
    ))

;;; Elhai table columns
;;; 
;;  gid 
;;  domain
;;  real-domain
;;  full-name
;;  organism-prefix
;;  org-nicknames
;;  gene-pattern
;;  alt-gene-pattern
;;  gene-prefix

;; No duplicate checking for anything in the elhai table! 
(defun elhai-data->seed-gid-table (data)
  (flet ((null-field->nil (x) 
           (if (or (null x) (every 'whitespacep x) (string= x "-")) nil x)
           ))
    (mapcar
     (lambda (record)
       (let* ((gid (first record))
              (name (fourth record))
              (prefix/nickname (fifth record))
              (nicknames-list-as-string (sixth record))
              (no-space-name (substitute #\- #\Space name))
              (no-space-prefix (substitute #\- #\Space prefix/nickname))
              (nicknames-list
               (if (equalp nicknames-list-as-string "-") 
                   nil
                 (mapcar 'string (read-from-string nicknames-list-as-string))))
              (gene-pattern (null-field->nil (seventh record)))
              (alt-gene-pattern (null-field->nil (eighth record)))
              (gene-prefix (null-field->nil (ninth record))))
         (list gid no-space-name no-space-prefix 
               (pushnew no-space-prefix nicknames-list :test 'string-equal)
               gene-pattern alt-gene-pattern gene-prefix
               )))
     data
     )))

(defun ramy-data->seed-gid-table (data)
  (mapcar 
   (lambda (record)
     (let* ((gid (first record))
            (name (second record))
            (prefix/nickname (third record))
            (no-space-name (substitute #\- #\Space name))
            (no-space-prefix (substitute #\- #\Space prefix/nickname)))
       (list gid no-space-name no-space-prefix (list no-space-prefix))
       ))
   data
   ))

;; Use all the gid entries in the elhai table and all the gid entries in the ramy
;; table that do not already have entries in the elhai table to create 
;; a single combined master table.  
(defun combine-elhai-and-ramy-tables (elhai-table ramy-table)
  (let ((elhai-hash (make-string-equal-hash-table)))
    (loop for record in elhai-table 
          do (setf (gethash (gid-slot record) elhai-hash) record))
    (append 
     (loop for record in ramy-table 
           as gid = (gid-slot record)
           as superseded? = (gethash gid elhai-hash)
           unless superseded?
           collect record
           )
     elhai-table 
     )))

;;; Goal is to build hash table of GID's -> organism data with
;;; no duplicates gids, gnames, prefixes or nicknames.

;;; Put GID's in hash keyed by GID
;;; Any duplicate GID's should be removed, first in gets to stay;
;;; keep track of duplicate GIDs, position and data in duplicates
;;; for subsequent warning messages.

;;; Remove all data in original list in duplicate positions.
;;; Do the same for gnames
;;; Do the same for prefixes
;;; Do similar for nicknames, but just remove duplicate nickname
;;; from data list.


(defun verify-gid-table-uniqueness ()

  (let ((table *seed-gid-table*)
        (gid-dup-info nil)
        (gid-dup-pos nil)
        (gname-dup-info nil)
        (gname-dup-pos nil)
        (prefix-dup-info nil)
        (prefix-dup-pos nil)
        (alias-external-dup-info nil)
        (alias-internal-dup-info nil)
        )

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
                           ;; (duplicate-pos original-pos dup-record)
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
                    "Duplicate ~A ~A in Seed GID table!"
                    "  At record pos. ~D, conflicting with record at pos. ~D.")
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
          (dups table 'gid-slot))
      (multiple-value-setq (gname-dup-info gname-dup-pos) 
          (dups table 'genome-name-slot))
      (multiple-value-setq (prefix-dup-info prefix-dup-pos) 
          (dups table 'prefix-slot))

      ;; make sure no prefixes internal '.'s 
      (loop for record in table 
            as prefix = (prefix-slot record)
            when prefix 
            do 
            (let ((pos (position #\. prefix)))
              (unless (or (null pos) (= pos (1- (length prefix))))
                (error
                 (one-string-nl
                  "No '.' allowed anywhere in any prefix except at end!"
                  "Organism ~A, prefix: ~S")
                 (gid-slot record) prefix
                 ))))

      ;; Deal with nicknames.  Blarf.
      ;; Check for internal duplicates as well as the normal
      ;; external duplicates.

      (loop 
       with hash = (make-string-equal-hash-table)
       for record in table
       for pos from 0
       as nicknames = (copy-list (nicknames-slot record))
       as internal-non-dups = (purge-duplicates nicknames :test 'string-equal)
       do
       (when (/= (length nicknames) (length internal-non-dups))
         (push (list pos (copy-tree record)) alias-internal-dup-info)
         (setf (nicknames-slot record) internal-non-dups)
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
          (setf (nicknames-slot record) internal-non-dups)
          )))

      ;; Report all GID duplicates
      (report-dups gid-dup-info "GIDs" "GID" 'gid-slot)
      ;; Report all Gname duplicates
      (report-dups gname-dup-info "gnames" "gname" 'genome-name-slot)
      ;; Report all Prefix duplicates
      (report-dups prefix-dup-info "Prefixes" "Prefix" 'prefix-slot)

      ;; Report all external nickname conflicts
      (loop for (dup-pos original-pos nickname data) in alias-external-dup-info
            for j from 0
            do
            (when (zerop j)
              (cformatt "Duplicate nicknames found in Seed GID table!")
              (cformatt "All nicknames across all organisms must be unique!~%"))
            (if (< j 10)
                (cformatt 
                 (one-string-nl
                  "Duplicate nickname ~A found associated with GID ~A !"
                  "  Conflicts with nickname given to GID ~A."
                  "  Duplicate record is at pos. ~D, conflicting with pos. ~D")
                 nickname (gid-slot data) (gid-slot (nth original-pos table))
                 dup-pos original-pos)
              (progn
                (cformatt
                 "...and ~D more duplicates!" 
                 (- (length alias-external-dup-info) 10))
                (return nil)
                )))

      (loop for (dup-pos data) in alias-internal-dup-info
            do
            (cformatt 
             (one-string-nl
              "Duplicated nicknames found in nicknames list for GID ~A:"
              "  Nicknames list: ~A "
              "  GID at position ~D in Seed GID table.")
             (gid-slot data) (nicknames-slot data) dup-pos
             ))

      (setq 
       table
       (remove-dups 
        table
        (union (union gid-dup-pos gname-dup-pos) prefix-dup-pos)
        ))

      (let ((final-hash (make-string-equal-hash-table)))
        (loop for record in table
              as gid = (gid-slot record)
              do
              (setf (gethash gid final-hash) record))
        (values final-hash table)
        )

      )))

             
(defun seed-gid-table->hash (table)
  (let ((hash (make-string-equal-hash-table)))
    (loop for data in table
          do
          (setf (gethash (gid-slot data) hash) data))
    hash
    ))
          
