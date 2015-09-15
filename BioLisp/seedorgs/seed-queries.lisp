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

(defun all-seed-viruses () (all-gids-in-domain "Virus"))

(defun all-seed-organisms () (all-gids-in-domain ""))

;; domain = "Bacteria"
;; returns ((gid1 description1) (gid2 description2) ...)
(defun all-gids-in-domain (domain &key (complete? nil) (restrictions nil))
  (ecase *seed-access-mode* 
    #+obsolete
    (:soap 
     (all-gids-in-domain-soap
      domain :complete? complete? :restrictions restrictions))
    (:mysql 
     (all-gids-in-domain-mysql
      domain :complete? complete? :restrictions restrictions
      ))))

(defun get-seed-annotation-info (gid)
  (case *seed-access-mode* 
    #+obsolete
    (:soap (get-seed-annotation-info-soap gid))
    (:mysql (get-seed-annotation-info-mysql gid))
    ))

;; Returns a list of lists of the form ((contig len) (contig len) ...)
(defun contig-info (gid)
  (ecase *seed-access-mode*
    #+obsolete
    (:soap (contig-info-soap gid))
    (:mysql (contig-info-mysql gid))
    ))

;; Returns a vector of pegs as strings
(defun all-pegs-of-a-genome (gid)
  (ecase *seed-access-mode* 
    #+obsolete
    (:soap (all-pegs-of-a-genome-soap gid))
    (:mysql (all-pegs-of-a-genome-mysql gid))
    ))

;; Returns ((seed-contig-name1 length1) (seed-contig-name2 length2) ...)
(defun get-seed-info-for-genome-tbl (gid orgname)
  (declare (ignore orgname))
  (contig-info gid)
  )

(defun get-seed-contig-length-info-for-genome (org-seed-id)
  (contig-info org-seed-id))

(defun get-seed-ncbi-info-for-contigs-of-org (org-seed-id)
  (ecase *seed-access-mode*
    (:mysql (ncbi-contig-info-mysql org-seed-id))
    ))
               

