;;; -*- Package: wlisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :wlisp)

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

;;; Author: JP Massar

(cl:defun specially-hacked-slotv-lambda? (form)
  (and (listp form)
       (= 2 (length form))
       (listp (first form))
       (= 3 (length (first form)))
       (destructuring-bind (lambda-symbol arglist slotv-call)
           (first form)
         (and (eq 'lambda lambda-symbol)
              (listp arglist)
              (= 1 (length arglist))
              (listp slotv-call)
              (or (find-package :frames) (find-package :aframes))
              (or 
               #+:jpmtf
               (and (find-package :swframes) 
                    (eq (find-symbol "SCALAR-HACK" :swframes)
                        (first slotv-call)))
               (eq (find-symbol "SLOTV" :frames) (first slotv-call))
               (eq (find-symbol "SLOTV" :aframes) (first slotv-call))
               )))))

(cl:defun transform-slotv-lambda-place-into-slotv-place (place)
  (let* ((lambda-function-form (first place))
         (target (second place))
         (slotv-form (third lambda-function-form))
         (slotv-symbol (first slotv-form))
         (slotv-slot (third slotv-form))
         )
    #+:jpmtf
    (if (and (find-package :swframes)
             (eq (find-symbol "SCALAR-HACK" :swframes) slotv-symbol))
        `(,(find-symbol "SLOTV" :frames) ,target ,slotv-slot)
      `(,slotv-symbol ,target ,slotv-slot)
      )
    #-:jpmtf
    `(,slotv-symbol ,target ,slotv-slot)
    ))
  

;;; This is redefined specifically to catch the frames #^ transformation
;;; of (setf (#^foo bar) baz) into 
;;; (setf ((lambda (x) (slotv x #$foo)) bar) baz)

;;; This undoes that transformation back into
;;; (setf (slotv bar #$foo) baz)

(cl:defmacro setf (&rest place-value-pairs)
  (let ((len (length place-value-pairs)))
    (cond
     ((zerop len) nil)
     ((not (evenp len))
      (error "Odd number of place/value pairs to SETF: ~A" place-value-pairs))
     ((not (= len 2))
      `(progn
         ,@(loop for list on place-value-pairs by #'cddr collect
                 `(setf ,(first list) ,(second list))
                 )))
     (t
      (let ((place (first place-value-pairs))
            (value (second place-value-pairs)))
        (verify-symbol-not-constant place) 
        ;; We need to redefine SETF because of the frames notation #^
        ;; Therefore, if the frames package is not yet defined
        ;; there is no reason to look for the specific 
        ;; transformation pattern we are trying to catch,
        ;; so just let LISP:SETF deal with everything.
        (if (not (find-package :frames)) 
            `(common-lisp:setf ,@place-value-pairs)
          ;; Look for (setf ((lambda (x) (frames:slotv x #$foo)) z) 5)
          (if (specially-hacked-slotv-lambda? place)
              ;; Transform to (common-lisp:setf (frames:slotv z #$foo) 5)
              `(common-lisp:setf
                ,(transform-slotv-lambda-place-into-slotv-place place)
                ,value)
            ;; No match.  Let LISP:SETF deal with it.
            `(common-lisp:setf ,place ,value)
            )))))))

(cl:defmacro setq (&rest var-value-pairs) 
  (let ((len (length var-value-pairs)))
    (cond
     ((zerop len) nil)
     ((not (evenp len))
      (error "Odd number of variable/value pairs to SETQ: ~A" 
             var-value-pairs))
     ((not (= len 2))
      `(progn
         ,@(loop for list on var-value-pairs by #'cddr collect
                 `(setq ,(first list) ,(second list))
                 )))
     (t 
      (verify-symbol-not-constant (first var-value-pairs))
      `(common-lisp:setq ,@var-value-pairs)
      ))))

(cl:defmacro multiple-value-setq ((&rest vars) form)
  (loop for var in vars do (verify-symbol-not-constant var))
  `(common-lisp:multiple-value-setq ,vars ,form)
  )
        

(cl:defmacro push (value place)
  (if (specially-hacked-slotv-lambda? place)
      `(common-lisp:push 
        ,value 
        ,(transform-slotv-lambda-place-into-slotv-place place))
    `(common-lisp:push ,value ,place)
    ))

(cl:defmacro pop (place)
  (if (specially-hacked-slotv-lambda? place)
      `(common-lisp:pop ,(transform-slotv-lambda-place-into-slotv-place place))
    `(common-lisp:pop ,place)
    ))

(cl:defmacro pushnew (value place &rest keys &key key test test-not)
  (declare (ignore key test test-not))
  (if (specially-hacked-slotv-lambda? place)
      `(common-lisp:pushnew
        ,value 
        ,(transform-slotv-lambda-place-into-slotv-place place)
        ,@keys)
    `(common-lisp:pushnew ,value ,place ,@keys)
    ))

(cl:defun verify-symbol-not-constant (x)
  (when (symbolp x) 
    (when (constantp x) 
      (error 
       (format 
        nil
        (concatenate 
         'string
         "You are trying to change the value of the symbol ~S.~%"
         "But ~S has been defined as a constant, whose value is ~S.~%"
         "You are not allowed to change the value of (or rebind) a variable~%"
         "which has been defined as a constant.~%")
        x x (symbol-value x)
        )))))


;;; DEFUN customization

(cl:defvar *customizers* (make-hash-table))
(cl:defvar *definers* ())

(cl:defun clear-customizers ()
  (cl:setf *customizers* (make-hash-table)))

(cl:defmacro define-definer (name)
  (let ((cl-name     (intern (symbol-name name) :cl))
	(original    (make-symbol "ORIGINAL"))
	(body        (make-symbol "BODY"))
	(environment (make-symbol "ENVIRONMENT")))
    (unless (not (eql cl-name name)) (error "Must shadow ~a" name))
    `(progn
       (cl:defmacro ,name 
                 (&whole ,original &body ,body &environment ,environment)
	 (if (gethash ',name *customizers*)
	     `(progn
		,@(loop with new-body = `(,',cl-name ,@,body) 
		     and pre-extra = ()
		     and post-extra = ()
		     for fn in (gethash ',name *customizers*) do 
		     (cl:setf (values new-body pre-extra post-extra) 
			      (funcall fn new-body ,environment ,original))
		     nconc pre-extra into all-pre-extra
		     nconc post-extra into all-post-extra
		     finally 
		     (return 
		       (append all-pre-extra (list new-body) all-post-extra)))
		',(first ,body))
	     `(,',cl-name ,@,body)))
       (cl:pushnew ',name *definers*))))

(cl:defmacro define-customizer (name (&rest lambda-list) what &body body)
  (let ((form (gensym))
	(environment (lambda-keyword-parameter '&environment lambda-list))
	(original (lambda-keyword-parameter '&original lambda-list))
	(params (remove-lambda-keywords '(&environment &original) lambda-list)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defun ,name (,form ,environment ,original)
	 (declare (ignorable ,environment ,original))
	 (destructuring-bind (,@params) ,form
	   ,@body))
       ,@(loop for thing in (parse-what what) collect
	      `(cl:pushnew ',name (gethash ',thing *customizers*))))))

(cl:defun parse-what (what)
  (cond
    ((eql what :all) *definers*)
    ((atom what) (list what))
    (t what)))

(cl:defun hash-keys (hash-table)
  (loop for k being the hash-keys of hash-table collect k))


(cl:defun lambda-keyword-parameter (lambda-keyword lambda-list)
  (or (cadr (member lambda-keyword lambda-list)) 
      (make-symbol (symbol-name lambda-keyword))))

(cl:defun remove-lambda-keywords (lambda-keywords lambda-list)
  (cond
    ((null lambda-list) nil)
    ((member (car lambda-list) lambda-keywords) 
     (remove-lambda-keywords lambda-keywords (cddr lambda-list)))
    (t (cons (car lambda-list) 
             (remove-lambda-keywords lambda-keywords (cdr lambda-list))))))


(define-definer defun)
(define-definer defvar)
(define-definer defparameter)
(define-definer defconstant)
(define-definer defmacro)