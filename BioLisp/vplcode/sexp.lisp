;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; Author: JP Massar, John Myers.

(defun proper-list-p (thing)
  (and (consp thing)
       (loop for cons on thing always (listp (cdr cons)))))

(defparameter *invalid-char-format* "{$ Unreadable character. Code = ~D $}")

(defun safe-string (string)
  (declare (simple-string string))
  (if (every 'standard-char-p string)
      string
    (with-output-to-string (p) 
      (loop for j fixnum from 0 
            for ch across string
            do 
            (if (standard-char-p ch)
                (princ ch p)
              (format p *invalid-char-format* (char-code ch))
              )))))

(defgeneric sexp->xexp (sexp)
  (:documentation 
   "Convert a sexp to an incredibly verbose sexp encoding
suitable to be fed into FOO's emit-xml function."))

(defmethod sexp->xexp ((sexp number)) `(:number ,(princ-to-string sexp)))
(defmethod sexp->xexp ((sexp string)) `(:string ,(safe-string sexp)))
(defmethod sexp->xexp ((sexp symbol))
  `((:symbol 
    ,@(unless (eql (symbol-package sexp) *default-symbol-package-for-xml*) 
        `(:package ,(package-name (symbol-package sexp)))))
    ,(symbol-name sexp)))

(defmethod sexp->xexp ((sexp cons))
  ;; This is not so efficient if given a long improper list.
  (if (proper-list-p sexp)
      `(:list ,@(mapcar #'sexp->xexp sexp))
      `(:cons ,(sexp->xexp (car sexp)) ,(sexp->xexp (cdr sexp)))))

(defun xexp->sexp (xexp)
  (case (car xexp)
    (:number (parse-integer (cadr xexp)))
    (:string (cadr xexp))
    (:symbol  (intern (second xexp) *default-symbol-package-for-xml*))
    (:list (mapcar #'xexp->sexp (rest xexp)))
    (:t 
     (let ((*package* *default-symbol-package-for-xml*)) 
       (apply #'read-from-string (rest xexp))))
    (:cons (apply #'cons (mapcar #'xexp->sexp (rest xexp))))
    (t
     (when (and (consp (car xexp)) (eql :symbol (caar xexp)))
       ;; Another flavor of :symbol
       (destructuring-bind ((tag package package-name) name) xexp
	 (declare (ignore tag package))
	 (values (intern name (find-package package-name))))))))

