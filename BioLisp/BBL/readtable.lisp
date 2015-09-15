;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

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

;;; Author: JP Massar. 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +lbs+ (intern "[" :keyword))
  (defconstant +rbs+ (intern "]" :keyword)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +lbc+ (char (symbol-name +lbs+) 0))
  (defconstant +rbc+ (char (symbol-name +rbs+) 0))
  (defconstant +lcc+ #\{) 
  (defconstant +rcc+ #\})) 

(defun bbl-readtable ()
  (let ((bt (copy-readtable *readtable*)))
    (set-macro-character +lbc+ 'bbl-left-bracket-reader-macro nil bt)
    (set-macro-character +rbc+ 'bbl-right-bracket-reader-macro nil bt)
    (set-macro-character +lcc+ 'bbl-left-curly-reader-macro nil bt)
    (set-macro-character +rcc+ 'bbl-right-curly-reader-macro nil bt)
    bt))

(defun bbl-left-bracket-reader-macro (stream char)
  (declare (ignore stream char))
  +lbs+)

(defun bbl-right-bracket-reader-macro (stream char)
  (declare (ignore stream char))
  +rbs+)

(let ((unique '(error "Unmatched right curly bracket '}' !!")))
  (defun bbl-left-curly-reader-macro (stream char)
    (declare (ignore char))
    `(bbi::%curly-list% 
      ,@(loop for form = (read stream t nil t)
              until (eq form unique) 
              collect form)))
  (defun bbl-right-curly-reader-macro (stream char)
    (declare (ignore stream char))
    unique))

(defmacro bbi::%curly-list% (&rest elems)
  `(lisp:list ,@elems))
  

(defparameter *bbl-readtable* (bbl-readtable))

;; save the standard pretty print table away so we can restore it 
;; when a user goes back to biolisp-mode 
(defparameter *biolisp-print-pprint-dispatch* *print-pprint-dispatch*)

(defparameter *bbl-print-pprint-dispatch* (copy-pprint-dispatch))

(defparameter *division-converts-rationals-to-floats* :single-float)

(let ((*print-pprint-dispatch* *bbl-print-pprint-dispatch*))
  (set-pprint-dispatch
   '(cons (member ref) t)
   (lambda (s o) 
     (format s "~S[" (second o))
     (loop for rest-of-indices on (cddr o)
           as index = (first rest-of-indices)
           do
           (format s "~S" index)
           (when (cdr rest-of-indices) (format s " "))
           finally (format s "]")
           )))
  (set-pprint-dispatch 
   '(cons (member bbi::%curly-list%) t)
   (lambda (s o) 
     (format s "{")
     (loop for elems on (rest o) 
           as elem = (first elems) 
           do
           (format s (if (null (cdr elems)) "~S" "~S ") elem)
           finally
           (format s (if (cdr o) "}" " }"))
           )))
  (set-pprint-dispatch 
   'ratio
   (lambda (s o) 
     (flet ((rf (x) (format s "~S/~S" (numerator x) (denominator x)))
            (sf (x) (format s "~S" (float x 0.0)))
            (df (x) (format s "~S" (float x 0.0d0))))
       (case *division-converts-rationals-to-floats* 
         ((:single-float :single t) 
          (handler-case (sf o)
            (floating-point-overflow 
             ()
             (handler-case (df o)
               (floating-point-overflow () (rf o))
               ))))
         ((:double-float :double) 
          (handler-case (df o)
            (floating-point-overflow () (rf o))
            ))
         (otherwise (rf o))
         ))))
  (set-pprint-dispatch
   'utils::garray
   (lambda (s o)
     (if *print-readably*       
         (utils::print-garray-readably o s)
       (progn
         (format 
          s
          "<Table ~A~Dd ("
          (let ((n (garray-named o)))
            (if n (formatn "named ~A " n) "")) 
          (garray-rank o))
         ;; (format s "<Table ~Dd (" (garray-rank o))
         (utils::print-garray-contents o s)
         (format s ")>")
         )))))