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

(defun seqidx-key (r) (first r))
(defun seqidx-header-start (r) (second r))
(defun seqidx-data-start (r) (third r))
(defun seqidx-seqlen (r) (fourth r))
(defun make-seqidx-record (key header-start data-start seqlen)
  (list key header-start data-start seqlen))

(defparameter *sequences-suffix* "sequences")
(defparameter *information-suffix* "information")
(defparameter *genome-directory* "genome")
(defparameter *genes-directory* "genes")
(defparameter *transcripts-directory* "transcripts")
(defparameter *proteins-directory* "proteins")
(defparameter *other-directory* "other")
(defparameter *documentation-directory* "documentation")



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




