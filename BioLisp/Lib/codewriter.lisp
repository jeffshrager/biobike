;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :user)

(defpackage :cw (:use :wlisp :biolisp :webuser :bioutils))

(in-package :cw)


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

;;; An auto-coder system for KnowOS.  Originally by Jeff Shrager

;;; FFF Have the REPLACE command indent the new code per the replaced element.

;;; The code writer stack is bound to the wb::*username*
;;; :code-writer-stack, and carefully manipulated.  (get
;;; wb::*username* :code-writer-stack)

;;; Remove and then push the :code command.

(wb::add-weblistener-keyword-command 
 :code 
 (lambda (s) (list 'code-writer s))
 "Issue auto-coder commands. For help: \":code help\"")

(defun code-writer (arg-string)
  (when (< (length arg-string) 6)
    (code-writer-help))
  (let* ((arg-string (subseq arg-string 6))
	 (split (string-split arg-string))
	 (keys (mapcar #'keywordize split))
	 (cmd (car keys))
	 (restkeys (cdr keys)))
    (case cmd
      (:where (code-where-was-i?))
      (:help (code-writer-help))
      (:undo (code-writer-undo))
      (:write (init-code-writer restkeys arg-string))
      (:replace (code-writer-replace restkeys arg-string))
      (t 
       (format t "~%~%Sorry; I didn't get that.  Here's help:~%~%")
       (code-writer-help))
      )))

(defun code-where-was-i? ()
  (set-and-push-new-code (pop (get (wb::user-session-id) :code-writer-stack)))
  "Here!")

(defun code-writer-undo ()
  (pop (get (wb::user-session-id) :code-writer-stack))
  (set-and-push-new-code (pop (get (wb::user-session-id) :code-writer-stack)))
  "Okay.")

(defun set-and-push-new-code (code)
  (push code (get (wb::user-session-id) :code-writer-stack))
  (setq wb::*multiline-form-data* code)
  )

;;; Each entry is: (string-for-matching short-string-for-help full-form)

(defparameter *code-writer-forms* 
  '(("define a new function using defun"
     "(DEFUN my-fn-name (args) body exprs ...)"
     #.(one-string-nl
	"(DEFUN my-fn-name (my-arg-1 my-arg-2 ...)"
	"  ;; Use as many exprs as needed.  They will be evaluated in sequence each time through the loop."
	"  body-expr-1"
	"  body-expr-2"
	"  ...etc..."
        "  ;; The value of the last body expr will be the value returned by the function!"
	"  )"
	""))
    ("a complex conditional cond"
     "(COND (test do-if-true) ...)"
     #.(one-string-nl
	"(COND (my-test-1 my-action-1)"
	"      (my-test-2 my-action-2)"
	"      ;; Use as many tests and actions as necessary."
	"      ...etc..."
	"      (T default-action) ; This is optional.  Often it will be a call to: (ERROR ...)"
	"    )"
	))
    ("an if then else conditional form"
     "(IF test what-to-do-if-true what-to-do-if-not-true)"
     #.(one-string-nl
	"(IF my-test"
	"    if-true-do-this ; This is NOT optional, although you can put NIL if you have no TRUE action."
	"    if-not-true-do-this ; This is optional."
	"    )"
	))
    ("simple loop over a list"
     "(LOOP FOR ... DO ... FINALLY ...)"
     #.(one-string-nl
	"(LOOP FOR my-var IN my-list"
	"      WHEN my-condition ; This line is optional and can be deleted if you don't need conditions on your loop."
	"      DO"
	"      ;; Use as many exprs as needed.  They will be evaluated in sequence each time through the loop."
	"      my-expr-1"
	"      my-expr-2"
	"      ...etc..."
	"      ;; The FINALLY clause is optional."
	"      FINALLY what-to-do-last"
	"      ;; (Note that simple loops return NIL unless you use an explicit (RETURN ...) call."
	"      ;;  This is often found in the FINALLY clause, but can be in the body of the loop as well."
	"      ;;  If you need to collect something on every loop cycle, you probably want a colleting loop,"
	"      ;;  not just a simple loop.)"
	"      )"
	))
    ("loop over a list collecting the results"
     "(LOOP FOR ... DO ... COLLECT ...)"
     #.(one-string-nl
	"(LOOP FOR my-var IN my-list"
	"      WHEN my-condition ; Delete this line if you dont need a condition."
	"      DO "
	"      ;; Use as many exprs as needed.  They will be evaluated in sequence each time through the loop."
	"      ;; (If you only need to collect, you don't need any exprs at all!)"
	"      myexpr-1"
	"      myexpr-2"
	"     ...etc..."
	"     COLLECT what-to-collect"
	"     )"
	))
    ))

;;; FFF Improve the parser so that it essentially uses word homology to ID the code to add!!

(defun init-code-writer (arg-keys arg-string)
  (declare (ignore arg-keys))
  (let ((form (guess-code-form (subseq arg-string 6))))
    (set-and-push-new-code form)
    "Okay."))

(defun code-writer-help ()
  (format t "All code writer commands are executed from the Enter box, following the keyword :CODE

Commands are:

  :code write a description of what to write   (this replaces the eval box completely with new code!)
  :code replace my-string with a description of what to write
  :code replace my-string with a call to a-specific-function-name
  :code undo
  :code where was I? (or just :code where)

Here are the things can the code writer knows how to write.  You can
use any phrases that approximates the description in the:
   \"a description of what to write\" 

part in the :code command. 

Description -> Abbreviated code (the real code is usually much longer!)
-----------    ----------------
")
  (loop for (name short nil) in *code-writer-forms*
	do (format t "~a -> ~a~%"  name short)))

(defun guess-code-form (arg-string)
  (let* ((score/descr (car (word-homology arg-string (mapcar #'car *code-writer-forms*))))
	 (score (car score/descr))
	 (descr (second score/descr))
	 (form (when (and descr (> score .5))
		 (third (find descr *code-writer-forms*
			       :test #'string-equal :key #'car))))
	 )
    (or form
      (error "I can't figure out what ~s refers to.  Use \":code help\" for help." arg-string))))

;;; Replace subcall several special forms, including:
;;; ... replace foo with any old description ...
;;; ... replace foo with a call to a-speific-function

(defun code-writer-replace (arg-keys arg-string)
  (unless (eq ':with (second arg-keys))
    (error ":code replace incorrect syntax.  Use: \":code replace something with description...\", or: \":code replace something with a call to a-specific-function\""))
  ;; Subdispatch based upon the word in the arg-keys
  ;; Skip the first two terms in arg-string (usually "my-thing" and "with") 
  (let* ((form (cond ((and (> (length arg-keys) 5)
			   (equal '(:a :call :to) (subseq arg-keys 2 5)))
		      (let ((fname (read-from-string (nth 6 (string-split arg-string)))))
			(print fname)
			(unless (ignore-errors (symbol-function fname))
			  (error "~a isn't the name of a known function!" fname))
			(format nil "(~a ~a" fname
				(subseq (WB::ARGLIST-TO-HELP-DISPLAY-STRING  
					 (WB::SYSTEM-SPECIFIC-ARGLIST-OF fname))
					1))))
		     (t (guess-code-form (subseq arg-string (+ 5 (search " with " arg-string)))))))
	 (target (string-downcase (string (first arg-keys))))
	 (mything-pos (search target wb::*multiline-form-data*))
	 )
    (unless mything-pos
      (error "Can't find ~a in the body of the code." target))
    (set-and-push-new-code 
	  (format nil "~a~a~a"
		  (subseq wb::*multiline-form-data* 0 mything-pos)
		  form
		  (subseq wb::*multiline-form-data* (+ mything-pos (length target))))
	    ))
    "Okay."
    )
