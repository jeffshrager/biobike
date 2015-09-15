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

(defparameter *organisms-seqinfo-rootdir* 
  (merge-pathnames 
   (make-pathname :directory `(:relative "data"))
   cl-user:*instance-bioetc-directory*
   ))

(defparameter *organisms-seqinfo-subdir* "seqinfo")
(defparameter *seqinfo-name* "sequence")
(defparameter *seqinfo-ext* "seq")
(defparameter *seqinfo-index-ext* "seqidx")
(defparameter *organisms-seq-testfile* "genes.test")

#|

(defun seqidx-key (r) (first r))
(defun seqidx-header-start (r) (second r))
(defun seqidx-data-start (r) (third r))
(defun seqidx-seqlen (r) (fourth r))
(defun make-seqidx-record (key header-start data-start seqlen)
  (list key header-start data-start seqlen))

(defvar *saved-old-organism-tables* nil)
(defvar *saved-organism-entry* nil)
(defvar *new-organism-tables* nil)
(defvar *upload-organism* nil)

(defvar *fasta-keys* nil)
(defvar *genome-keys* nil)
(defvar *genes-keys* nil)
(defvar *transcripts-keys* nil)
(defvar *proteins-keys* nil)
(defvar *organism-plist* nil)

(defparameter *standard-key-max-length* 128)
(defparameter *organisms-table-name* "new_organisms")
(defparameter *organisms-table-key* "name")
(defparameter *organism-tables-table-key* "table_name")
(defparameter *fasta-storage-table-key* "name")
(defparameter *generic-information-table-key* "name")
(defparameter *generic-properties-field* "properties")
(defparameter *generic-description-field* "description")
(defparameter *saved-table-prefix* "temp_saved_")
(defparameter *tables-table-suffix* "_database_tables")

|#

(defparameter *sequences-suffix* "sequences")
(defparameter *information-suffix* "information")
(defparameter *genome-directory* "genome")
(defparameter *genes-directory* "genes")
(defparameter *transcripts-directory* "transcripts")
(defparameter *proteins-directory* "proteins")
(defparameter *other-directory* "other")
(defparameter *documentation-directory* "documentation")

#|

;; The name of the table for a given organism that contains the 'list'
;; of tables in the database that pertain directly to this organism.
(defun organism-tables-table-name (organism-name)
  (format nil "~A~A" organism-name *tables-table-suffix*))

(defun sequences-table-name (organism-name data-type-name)
  (one-string organism-name "_" data-type-name "_" *sequences-suffix*))
(defun information-table-name (organism-name data-type-name)
  (one-string organism-name "_" data-type-name "_" *information-suffix*))
(defun other-sequences-table-name (fasta-file)
  (let ((filename (pathname-name (pathname fasta-file))))
    (one-string *upload-organism* "_other_" filename "_fasta")
    ))
(defun genome-seq-tbl (orgn) (sequences-table-name orgn *genome-directory*))
(defun genome-info-tbl (orgn) (information-table-name orgn *genome-directory*))
(defun genes-seq-tbl (orgn) (sequences-table-name orgn *genes-directory*))
(defun genes-info-tbl (orgn) (information-table-name orgn *genes-directory*))
(defun transcripts-seq-tbl (orgn) 
  (sequences-table-name orgn *transcripts-directory*))
(defun transcripts-info-tbl (orgn) 
  (information-table-name orgn *transcripts-directory*))
(defun proteins-seq-tbl (orgn) 
  (sequences-table-name orgn *proteins-directory*))
(defun proteins-info-tbl (orgn) 
  (information-table-name orgn *proteins-directory*))

(defparameter *generated-temp-file-types* '("fsl" "fasl" "fidx"))

|#

(defun organism-etc-directory (orgn)
  ;; Cannot create an entire logical pathname string, then
  ;; translate it, because ORGN might have '_' and other
  ;; verboten chars in it.  Barf.
  (merge-pathnames
   (make-pathname :directory `(:relative ,(string-downcase orgn)))
   *organisms-seqinfo-rootdir*
   ))

(defun organism-seq-testfile (orgn)
  (merge-pathnames 
   (make-pathname 
    :directory `(:relative ,*documentation-directory*)
    :name *organisms-seq-testfile*)
   (organism-etc-directory orgn)))

(defun seqinfo-directory (orgn)
  ;; Cannot create an entire logical pathname string, then
  ;; translate it, because ORGN might have '_' and other
  ;; verboten chars in it.
  (merge-pathnames
   (make-pathname :directory `(:relative ,*organisms-seqinfo-subdir*))
   (organism-etc-directory orgn)
   ))

