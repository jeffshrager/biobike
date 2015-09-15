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


(defparameter *seed-genome-table-column-indices* 
  '((:genome . 0) (:gname . 1) (:szdna . 2)
    (:cksum . 3) (:maindomain . 4) (:pegs . 5)
    (:rnas . 6) (:complete . 7) (:restrictions . 8)
    (:taxonomy . 9)
    ))

(defparameter *not-yet-used-seed-genome-table-columns*
  '(:cksum))

;; ("genome" "gname" "szdna" "cksum" "maindomain" "pegs" "rnas" "complete"
;; "restrictions" "taxonomy")

(defun ggtrv (record key)
  (nth (cdr (assoc key *seed-genome-table-column-indices*)) record))

(defun genome-slot (x) (ggtrv x :genome))
(defun gname-slot (x) (ggtrv x :gname))
(defun szdna-slot (x) (ggtrv x :szdna))
(defun cksum-slot (x) (ggtrv x :cksum))
(defun maindomain-slot (x) (ggtrv x :maindomain))
(defun pegs-slot (x) (ggtrv x :pegs))
(defun rnas-slot (x) (ggtrv x :rnas))
(defun complete-slot (x) (ggtrv x :complete))
(defun restrictions-slot (x) (ggtrv x :restrictions))
(defun taxonomy-slot (x) (ggtrv x :taxonomy))

(defun process-seed-genome-data ()
  (multiple-value-bind (genome-data genome-columns)
      (if user::*real-seed-available?* 
          (all-genomes-of-the-seed)
        (read-aux-seed-genome-info))
    (verify-genome-headers-against-keywords genome-columns)
    (write-aux-seed-genome-info genome-data genome-columns)
    (labels ((toint (x) 
               (let ((result (ignore-errors (parse-integer x))))
                 (if (null result) -1 result)
                 ))
             (toboolean (x)
               (let ((result (toint x)))
                 (case result
                   (-1 nil)
                   (0 nil)
                   (otherwise t)
                   ))))
      (let ((transformed-genome-data 
             (mapcar 
              (lambda (record) 
                ;; Transform each item in a seed genome record 
                ;; if appropriate...Only certain items need to
                ;; be processed; those not processed are left as is
                (loop for datum in record for j from 0 
                      as key =
                      (car (rassoc j *seed-genome-table-column-indices*))
                      collect
                      (case key 
                        (:complete (toboolean datum))
                        (:restrictions (toboolean datum))
                        (otherwise datum)
                        )))
              genome-data
              )))
        (setq *seed-genome-table-info* transformed-genome-data)
        (setq *seed-genome-table-hash* (make-string-equal-hash-table))
        (loop for record in *seed-genome-table-info* 
              as key = (genome-slot record)
              do
              (when (gethash key *seed-genome-table-hash*)
                (warn "Duplicate gid ~A from the seed!!" key))
              (setf (gethash key *seed-genome-table-hash*) record)
              ))
      (compare-previous-seed-genome-table)
      )))
              

(defun verify-genome-headers-against-keywords (headers)
  (let ((header-keys (mapcar 'keywordize headers)))
    ;; Check that we know about every column in the genome table
    (loop for key in header-keys 
          for j from 0 
          as index = (cdr (assoc key *seed-genome-table-column-indices*))
          do
          (cond
           ((null index) 
            (warn
             (one-string-nl
              "The seed genome table has a column titled ~A"
              "but the code is not cognizant of that column heading.")
             key
             ))
           ((/= index j)
            (error 
             (one-string-nl
              "The seed genome table column titled ~A is at column ~D"
              "but the code thinks that it should be in column ~D")
             key j index
             ))
           (t nil)
           ))
    ;; Check that every column the code knows is in fact in the genome table
    (loop for (key . nil) in *seed-genome-table-column-indices*
          do
          (unless (find key header-keys)
            (funcall 
             (if (member key *not-yet-used-seed-genome-table-columns*)
                 'warn
               'error)
             (one-string-nl
              "The code believes there should be a column titled ~A"
              "but the seed genome table has no such column.")
             key
             )))))

(defun read-aux-seed-genome-info ()
  (with-open-file (p *aux-seed-genome-info-path* :direction :input)
    (let ((data (read p)))
      (values (rest data) (first data))
      )))

(defun write-aux-seed-genome-info (genome-data genome-columns)
  (ignore-errors 
    (with-open-file 
        (p *aux-seed-genome-info-path* :direction :output :if-exists :supersede)
      (print (cons genome-columns genome-data) p)
      )))

(defun compare-previous-seed-genome-table ()
  (let* ((previous-gids (slotv #$seed-genome-table-gids #$list))
         (gids 
          (lmaphash
           (lambda (key value) (declare (ignore value)) key)
           *seed-genome-table-hash*
           ))
         (deleted-gids (set-difference previous-gids gids :test 'string-equal))
         (added-gids (set-difference gids previous-gids :test 'string-equal)))
    (when added-gids
      (cformatt 
       "~D gids were added to the seed genome table since previous boot!" 
       (length added-gids))
      (report-added-gids added-gids))
    (when deleted-gids
      (cformatt 
       "~D gids were deleted from the seed genome table since previous boot!" 
       (length deleted-gids))
      (report-deleted-gids deleted-gids))
    (cformatt "Storing seed genome table gids for posterity...")
    (setf (slotv #$seed-genome-table-gids #$list) gids)
    nil
    ))