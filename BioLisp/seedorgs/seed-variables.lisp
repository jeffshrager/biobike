;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

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

#+:lispworks
(defpackage :db.ac (:use :lisp))

;; all the data found in the external master table 
(defvar *external-seed-master-table* nil)
;; all the non-problematic data (non-duplicates, no errors) 
;; from the master table
(defvar *external-seed-master-hash* nil)
(defvar *existing-acache-genome-frames* nil)
(defvar *updated-seed-genome-frames* nil)
(defvar *stable-seed-genome-frames* nil)
;; all the genome data as downloaded from the SEED.
(defvar *seed-genome-table-hash* nil)
;; A list of all organisms in the master list and also found in the SEED.
(defvar *available-organisms* nil)


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

(defparameter *master-table-column-indices* 
  '((:gid . 0) (:domain . 1) (:real-domain . 2)
    (:full-name . 3) (:organism-prefix . 4) (:org-nicknames . 5)
    (:gene-pattern . 6) (:alt-gene-pattern . 7) (:gene-prefix . 8)
    (:provisional . 9)
    ))

(defparameter *master-table-ncolumns* (length *master-table-column-indices*))

(defparameter *not-yet-used-master-table-columns* 
  '(:provisional)
  )

(defvar *enable-gene-pattern-msf* t)
(defvar *compiled-gene-pattern* nil)
(defvar *compiled-alt-gene-pattern* nil)


(defvar *loaded-metagenomes* nil)

(defvar *available-metagenomes* nil)