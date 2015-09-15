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

(defparameter *bbl-documentation-toplevel-url* 
  "http://ramsites.net/~biobike/help/index.html")

(defparameter *bbl-frame-mapping* nil 
  #.(one-string-nl
     "A mapping from names to frames.  The BBL instance defines this mapping"
     "in the .../bike/bbl-instance.lisp file."))

;; Error message format string creator for error messages intended for
;; BBL users.  

;; This generates a string if all the arguments are strings or designated
;; keywords; otherwise it generates code that generates a string after 
;; evaluating the non-string/keyword arguments at execution time.  

(defmacro serr+ (&rest arguments)
  (let* ((problem "~%*** PROBLEM: ")
         (advice  "~&*** ADVICE:  ")
         (newline "~&   ")
         (indent  "~&             ")
         (help~a  "~&*** MORE HELP: Enter (help ~a).~%")
         (substituted-args 
          (loop for argument in arguments
                collect 
                (cond 
                 ((symbol= argument :problem) problem)
                 ((symbol= argument :advice) advice)
                 ((symbol= argument :indent) indent)
                 ((symbol= argument :newline) newline)
                 ((symbol= argument :help~a) help~a)
                 (t argument))
                )))
    (if (every 'stringp substituted-args)
        (string-join substituted-args)
      `(string-join (list ,@substituted-args))
      )))

;;; This generates an actual error after composing
;;; an error message.  It is similar to serr+ above except
;;; that it takes an additional FORMAT-ARGS keyword; all arguments
;;; after this keyword are used as format arguments to the error message
;;; composed as with serr+.  This allows a single form to be used to generate
;;; a bbl error instead of the original, clumsy 
;;; (error (s+ (err+ ...)))


(defmacro err+ (&rest arguments)
  (let* ((f-args nil)
         (problem "~%~%*** PROBLEM: ")
         (advice  "~&~%*** ADVICE:  ")
         (newline "~&   ")
         (indent  "~&             ")
        ; (help~a  "~&~%*** MORE HELP: Enter (help ~a).~%")
         (help~a  "~&~%*** MORE HELP: Try clicking HELP in the action menu of ~A.~%")
         (substituted-args 
          (loop for argument in arguments
                for j from 0
                until f-args
                if (symbol= argument :format-args)
                do
                (setf f-args (subseq arguments (1+ j)))
                else
                collect 
                (cond 
                 ((symbol= argument :problem) problem)
                 ((symbol= argument :advice) advice)
                 ((symbol= argument :indent) indent)
                 ((symbol= argument :newline) newline)
                 ((symbol= argument :help~a) help~a)
                 (t argument))
                )))
    (if (every 'stringp substituted-args)
        `(error ,(string-join substituted-args) ,@f-args)
      `(error (string-join (list ,@substituted-args)) ,@f-args)
      )))




