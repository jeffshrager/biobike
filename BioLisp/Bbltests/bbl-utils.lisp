;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbl-test-user; -*-

(in-package :bbl-test-user)

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

;;; Author: JP Massar, Mark Slupesky.

;; stuff for bbl-pre-tests

(lisp:defvar *g*)

(lisp:eval-when (:compile-toplevel :load-toplevel :execute)
  (lisp:defvar nothing nil)
  (lisp:defvar nothing2 nil)
  (lisp:defvar deffun nil))

(lisp:makunbound 'nothing)
(lisp:makunbound 'nothing2)
(lisp:makunbound 'deffun)


(defmacro defbbltest (name &body body)
  `(tests:deftest 
    ,name 
    (with-bbl-form ,(lisp:first body)) ,@(cdr body) :chapter :bbl-basic))

(defmacro defbbltest= (name &body body)
  `(tests:deftest ,name (bbi::with-bbl-form ,(lisp:first body)) ,@(cdr body) 
                  :chapter :bbl-basic :comparison 'equal))

(defmacro defreplacetest= (name &body body)
  `(tests:deftest 
    ,name 
    (with-bbl-form ,(lisp:first body)) ,@(cdr body) 
    :chapter :bbl-replace :comparison 'equal))

(defmacro defbbltest== (name &body body)
  `(tests:deftest 
    ,name
    (with-bbl-form ,(lisp:first body)) ,@(cdr body) 
    :chapter :bbl-basic :comparison 'equalp))

(defmacro defbblerrortest= (name test-form condition &rest goo)
  `(tests:deferrortest 
    ,name (with-bbl-form ,test-form) ,condition 
    :chapter :bbl-basic :comparison 'equal ,@goo))

(defmacro defreplaceerrortest= (name test-form condition &rest goo)
  `(tests:deferrortest 
    ,name (with-bbl-form ,test-form) ,condition 
    :chapter :bbl-replace :comparison 'equal ,@goo))

(defmacro defbblorgtest= (name form expected &rest goo)
  `(defbbltest= ,name 
                (if (no-orgs) "" (with-bbl-form ,form))
                (if (no-orgs) "" ,expected)
                ,@goo
                :chapter :bbl-org :comparison 'equal))

(defmacro defbblssttest= (name form expected &rest goo)
  `(defbbltest= ,name 
                (if (no-orgs) "" (with-bbl-form ,form))
                (if (no-orgs) "" ,expected)
                ,@goo
                :chapter :bbl-sst :comparison 'equal))

(defmacro defbblorgerrortest= (name form condition &rest goo)
  `(tests:deftest 
    ,name
    (handler-case 
        (if (no-orgs) ""
          (progn (with-bbl-form ,form)
            :form-returned-without-signalling-condition))
      (,condition () :form-signalled-condition))
    (if (no-orgs) "" :form-signalled-condition)
    :chapter :bbl-org :comparison 'equal ,@goo))

#+not-used
(defmacro defbblptest= (name predicate form expected &rest goo)
  `(defbbltest= ,name 
                (if ,predicate "" ,form)
                (if ,predicate "" ,expected)
                ,@goo
                :chapter :bbl :comparison 'equal))


;; stuff for bbl-org-tests


(defun no-orgs ()
  (null (bio::loaded-organisms)))


;; FIRST-x-IN-SYSTEM is just (#^x ...)
;; so -assumption- first-organism in the first-contig of the first-organism
;; is incorrect
(defun first-gene-in-system ()
  (unless (no-orgs)
    (lisp:first (#^genes (lisp:first (bio::loaded-organisms)))))) 
(defun first-protein-in-system ()
  (unless (no-orgs)
    (lisp:first (#^proteins (lisp:first (bio::loaded-organisms))))))
(defun first-contig-in-system ()
  (unless (no-orgs)
    (lisp:first (#^contiguous-sequences (lisp:first (bio::loaded-organisms))))))

(defun first-large-contig-in-system (&optional (min 10))
  (block exit
    (unless (no-orgs)
      (loop for org in (bio::loaded-organisms) do 
        (loop for contig in (#^contiguous-sequences org) do 
          (when (>= (length (#^genes-sorted-by-position contig)) min)
            (return-from exit contig))))
      (error "Could not find contig with at least ~D genes!" min))))
(defun first-organism-in-system ()
  (unless (no-orgs)
    (lisp:first (bio::loaded-organisms))))
(defun first-circular-contig-in-system ()
  (unless (no-orgs)
    (block exit
      (loop for org in (bio::loaded-organisms) do
        (loop for contig in (#^contiguous-sequences org)
          as c? = (#^circular contig)
          when c? (return-from exit contig))))))
(defun first-non-circular-contig-in-system ()
  (unless (no-orgs)
    (block exit
      (loop for org in (bio::loaded-organisms) do
        (loop for contig in (#^contiguous-sequences org)
          as not-c? = (not (#^circular contig))
          when not-c? (return-from exit contig))))))
(defun first-organism-with-one-contig-in-system ()
  (unless (no-orgs)
    (block exit
      (loop for org in (bio::loaded-organisms)
        as lc = (= (length (#^contigous-sequences org)) 1)
        when lc (return-from exit org)))))
(defun first-organism-with-multiple-contigs-in-system ()
  (unless (no-orgs)
    (block exit
      (loop for org in (bio::loaded-organisms)
        as lc = (> (length (#^contigous-sequences org)) 1)
        when lc (return-from exit org)))))
(defun first-non-protein-encoding-gene ()
  (unless (no-orgs)
    (block exit
      (loop for org in (bio::loaded-organisms) do
        (loop for gene in (#^genes org) 
          as no-encode? = (not (#^encodes-protein gene))
          when no-encode? (return-from exit gene))))))



(defun length-of-gene (g)
  (+ 1 
     (- (#^to g) (#^from g))))
       
(defun middle-coordinate (g)
  (floor (/ (+ (#^from g) (#^to g)) 2)))