;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

;; sketch of CPS compiler. For the moment compiles to Lisp but could be retargeted to emit Javascript.

(defun self-evaluating-p (thing)
  (typep thing '(or symbol number string)))

(defun special-form-p (thing)
  (get (car thing) 'special-form-compiler))

(defun macro-form-p (thing)
  (get (car thing) 'macroexpander))

(defun primitive-p (fn-name) (member fn-name '(+ - * /)))

(defun compile-special-form (thing continuation)
  (funcall (get (car thing) 'special-form-compiler) thing continuation))

(defun macroexpand-thing (thing)
  (funcall (macro-expander (car thing)) thing))

(defun macro-expander (name)
  (get name 'macroexpander))

(setf (get 'if 'special-form-compiler) 'compile-if)
(setf (get 'progn 'special-form-compiler) 'compile-progn)
(setf (get 'quote 'special-form-compiler) 'compile-quote)

(defun compile-thing (thing continuation)
  (cond
    ((self-evaluating-p thing) (compile-self-evaluating thing continuation))
    ((special-form-p thing)    (compile-special-form thing continuation))
    ((macro-form-p thing)      (compile-thing (macroexpand-thing thing) continuation))
    ((consp thing)             (compile-funcall thing continuation))
    (t (error "Don't know how to compile ~s" thing))))

(defun compile-self-evaluating (symbol continuation)
  `(,continuation ,symbol))

(defun compile-funcall (form continuation)
  (destructuring-bind (fn &rest args) form
    (labels ((c (args params)
	       (cond
		 ((null args)
		  (if (primitive-p fn)
		      `(funcall ,continuation (,fn ,@(reverse params)))
		      `(,fn ,continuation ,@(reverse params))))
		 (t
		  (let ((new-param (gensym)))
		    (compile-thing
		     (first args)
		     `(lambda (,new-param) ,(c (rest args) (cons new-param params)))))))))
      (c args ()))))

(defun compile-if (form continuation)
  (destructuring-bind (if condition then &optional else) form
    (declare (ignore if))
    (let ((param (gensym)))
      (compile-thing condition 
		     `(lambda (,param) 
			(if ,param
			    ,(compile-thing then continuation)
			    ,(if else
				 (compile-thing else continuation)
				 `(,continuation nil))))))))

(defun compile-progn (form continuation)
  (labels ((c (forms latest-param)
	     (if forms
		 (let ((new-param (gensym)))
		   (compile-thing
		    (first forms)
		    `(lambda (,new-param) ,(c (rest forms) new-param))))
		 `(funcall ,continuation ,latest-param))))
      (c (rest form) nil)))

(defun compile-quote (form continuation)
  `(funcall ,continuation ',(cadr form)))

