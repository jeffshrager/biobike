;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

;;; BUGS:

;;; closures over "this" don't always work. For instance,
;;;
;;; (defmethod (Foo a-method) ()
;;;  (set (@ someobject onclick) (lambda () this)))
;;;
;;; sets up an event handler which is a method on 'someobject' so it
;;; will return someobject rather than the Foo object which is 'this'
;;; in a-method. Blech.

(in-package :com.gigamonkeys.foo.lispscript)

(defclass lispscript (javascript)
  ()
  (:default-initargs
    :input-package (find-package :com.gigamonkeys.foo.lispscript)))

(defparameter *lispscript* (make-instance 'lispscript))

;;; Compilation environment

(defmethod top-level-environment ((language lispscript))
  (new-env 'needs-value t (call-next-method)))

(defun needs-value (env)
  (or (cdr (assoc 'needs-value env))
      (eql (com.gigamonkeys.foo.javascript::statement-or-expression env) :expression)))

(defun find-open-block (name env &key (expected t))
  (let ((cons (assoc `(open-block ,name) env :test #'equal)))
    (cond
      ((and expected (not cons))
       (error "No open block named ~a in environment ~s" name env))
      #+(or)((and (not expected) cons)
       (error "Already an open block named ~a in environment ~s" name env))
      (t cons))))

(defun add-open-block (name env)
  (find-open-block name env :expected nil)
  (new-env `(open-block ,name) nil env))

(defun mark-block-return (name env)
  (let ((cons (find-open-block name env :expected t)))
    (setf (cdr cons) t)))

(defun block-returned-p (name env)
  (let ((cons (find-open-block name env :expected t)))
    (cdr cons)))
  
;;; Sub-primitive special operators. Not meant to be used directly in
;;; user code but handy for implementing primitive special operators
;;; and "built-in" macros.

(define-javascript-macro needs-value (&body body)
  `(augment-environment ((needs-value . t)) ,@body))

(define-javascript-macro discard-value (&body body)
  `(augment-environment ((needs-value . nil)) ,@body))

(define-javascript-macro return-last (&body body)
  `(prog (discard-value (prog ,@(butlast body))) (needs-value (return ,@(last body)))))

(define-javascript-macro scope ((&rest bindings) &body body)
  `(|.call|
    (function (,@(mapcar #'first bindings)) ,@body)
    |this| ,@(mapcar #'second bindings)))

;;; Language constructs. These are the "special-operators" of the
;;; Lispscript language.

(define-javascript-macro |let| ((&rest bindings) &body body &environment env)
  `(scope (,@bindings)
	  ,(if (needs-value env)
	       `(return-last ,@body)
	       `(discard-value (prog ,@body)))))

(define-javascript-macro |let*| ((&rest bindings) &body body)
  (if bindings
      `(|let| (,(first bindings))
	      ,@(if (rest bindings)
		    `((|let*| (,@(rest bindings)) ,@body))
		    body))
      `(|let| () ,@body)))


#|

Bah! To make closures work properly relative to `this', so you
can do things like:

  (defmethod (Widget whatever)
    (lambda () (something this)))

we need to jump through all kinds of hoops to return a closure
that gets passed the appropriate object and then when called
CALLs the actual closure function with that object as the
thisObj.

Here's a sketch of the needed Javascript code:

function Widget () {
    return this;
}

Widget.prototype.aMethod = function () { return this; };
Widget.prototype.aClosure = function () { 
    return (function (gensym) {
      return function () {
        return (function () { return this; }).call(gensym);
      };
    })(this);
}

|#

(define-javascript-macro |lambda| ((&rest params) &body body)
  `(function ,params (return-last ,@body)))

(define-javascript-macro |progn| (&body body &environment env)
  (if (needs-value env)
    `(|let| () ,@body)
    `(prog ,@body)))

(define-javascript-macro |block| (name &body body &environment env)
  (let ((e (javascript-gensym))
	(return-var (block-name name)))
    (if (not (returned-p return-var body env))
	`(|progn| ,@body)
	`(augment-environment
	  (((open-block ,return-var) . nil))
	  ,(if (needs-value env)
	       `((function
		  (,return-var)
		  (try 
		   (return (|progn| ,@body))
				       (catch ,e
					 (if (|eq| ,e ,return-var)
					     (return (@ ,return-var |value|))
					     (throw ,e)))))
		 (object |value| |undefined|))
	       `((function
		  (,return-var)
		  (try (prog ,@body)
		       (catch ,e
			 (if (|eq| ,e ,return-var)
			     (return (@ ,return-var |value|))
			     (throw ,e)))))
		 (object |value| |undefined|)))))))

(defun block-name (symbol) 
  (intern (format nil "block$~a" (symbol-name symbol)) :keyword))

(defun returned-p (block-name body env)
  (loop with new-env = (add-open-block block-name env)
     for form in body
     do (process *lispscript* nil form new-env)
     thereis (block-returned-p block-name new-env)))


(define-javascript-macro |return-from| (name value &environment env)
  (mark-block-return (block-name name) env)
  (if (needs-value env)
      `(|progn|
	(|set| (@ ,(block-name name) |value|) ,value)
	((function () (throw ,(block-name name)))))
      `(prog
	   (|set| (@ ,(block-name name) |value|) ,value)
	  (throw ,(block-name name)))))

(define-javascript-macro |return| (value)
  `(|return-from| |nil| ,value))

;; Scheme style -- internal DEFUN's are local
#+(or)(define-javascript-macro |defun| (name (&rest params) &body body)
  `(function ,name ,params (return (|block| ,name ,@body))))

;; Common Lisp style -- even internal DEFUN's are gobal
(define-javascript-macro |defun| (name (&rest params) &body body)
  `(|set| (@ |window| ,name) (function ,params (return (|block| ,name ,@body)))))

;; Scheme style
#+(or)(define-javascript-macro |defvar| (name &optional value)
  `(VAR ,name ,@(when value `(,value))))

;; Common Lisp style
(define-javascript-macro |defvar| (name &optional value)
  `(|set| (@ |window| ,name) ,@(when value `(,value))))

(define-javascript-macro |if| (test then &optional else &environment env)
  (if (needs-value env)
      `(scope () (if ,test (block (return ,then)) (block (return ,(if else else '|undefined|)))))
      `(if ,test (block ,then) ,@(if else `((block ,else))))))

(define-javascript-macro |cond| (&body clauses)
  (destructuring-bind ((test &body body) &rest clauses) clauses
    `(|if| (needs-value ,test)
	   (|progn| ,@body)
	   ,@(when clauses
		`((|cond| ,@clauses))))))

(define-javascript-macro |when| (test &body body)
  `(|if| ,test (|progn| ,@body)))

(define-javascript-macro |unless| (test &body body)
  `(|if| (! ,test) (|progn| ,@body)))

(define-javascript-macro |and| (first &rest rest)
  `(&& ,first ,@rest))

(define-javascript-macro |or| (first &rest rest)
  `(\|\| ,first ,@rest))

(define-javascript-macro |not| (form)
  `(! ,form))

(define-javascript-macro |do| ((&rest variables) (end-test-form &optional (result-form '|undefined|)) &body body)
  (multiple-value-bind (bindings step-forms) (parse-do-variables variables)
    `(|block| |nil|
	      (|let| (,@bindings)
		     (while (! ,end-test-form)
		       (block ,@body
			 ,@step-forms))
		     ,@(when result-form (list result-form))))))

(define-javascript-macro |do*| ((&rest variables) (end-test-form &optional result-form) &body body)
  (multiple-value-bind (bindings step-forms) (parse-do-variables variables)
    `(|block| |nil|
	      (|let*| (,@bindings)
		      (while (! ,end-test-form)
			(block ,@body
			  ,@step-forms))
		      ,@(when result-form (list result-form))))))

(define-javascript-macro |dotimes| ((var iterations) &body body)
  (let ((iterations-value (javascript-gensym)))
    `(|block| |nil|
	      (var ,iterations-value ,iterations)
	      (|for| ((var ,var 0) (< ,var ,iterations-value) (++ ,var :post)) ,@body))))

(define-javascript-macro |dolist| ((var sequence) &body body &environment env)
  (let* ((idxvar (javascript-gensym))
	 (sequence-value (javascript-gensym)))
    `(|block| |nil| 
	      (var ,sequence-value ,sequence)
	      (for (,idxvar in ,sequence-value)
		   (block (discard-value (|let| ((,var (ref ,sequence-value ,idxvar))) ,@body))))
	      ,@(if (needs-value env) '(|undefined|)))))

(define-javascript-macro |doslots| (((name value) sequence) &body body &environment env)
  (let* ((sequence-value (javascript-gensym)))
    `(|block| |nil| 
	      (var ,sequence-value ,sequence)
	      (for (,name in ,sequence-value)
		   (block (discard-value (|let| ((,value (ref ,sequence-value ,name))) ,@body))))
	      ,@(if (needs-value env) '(|undefined|)))))

(define-javascript-macro |while| (condition &body body &environment env)
  `(|block| |nil| (while ,condition (block (discard-value ,@body))) |undefined|))

(define-javascript-macro |until| (condition &body body)
  `(|while| (! ,condition) ,@body))


(defun parse-do-variables (variables)
  (flet ((normalize (variable)
	   (let ((as-cons (if (symbolp variable) (list variable) variable)))
	     (loop repeat 3 collect (pop as-cons)))))
    (loop for (var init step) in (mapcar #'normalize variables)
	 collect (list var (or init '|undefined|)) into bindings
	 when step collect `(|set| ,var ,step) into step-forms
	 finally (return (values bindings step-forms)))))
		   
(define-javascript-macro |for| ((var test step) &body body &environment env)
  (if (needs-value env)
    `(|progn|
      (for (,var ,test ,step) (block ,@body))
      |undefined|)
    `(for (,var ,test ,step) (block ,@body))))


(define-javascript-macro |new| (type &rest args)
  `(NEW ,type ,@args))

(define-javascript-macro |ignore-errors| (&body body &environment env)
  (let ((e (javascript-gensym)))
    (if (needs-value env)
	`((function () (try (return-last ,@body) (catch ,e (return |false|)))))
	`(try ,@body (catch ,e)))))

(define-javascript-macro |defmethod| ((class name) (&rest parameters) &body body)
  `(|set| (@ ,class |prototype| ,name)  (function ,parameters  (return (|block| ,name ,@body)))))

(define-javascript-macro |define-class-method| ((class name) (&rest parameters) &body body)
  `(|set| (@ ,class ,name)  (function ,parameters  (return (|block| ,name ,@body)))))

(define-javascript-macro |ref| (var &rest slots)
  `(ref ,var ,@slots))

(define-javascript-macro |array| (var &rest slots)
  `(array ,var ,@slots))

(define-javascript-macro |case| (value &body clauses)
  `(scope ()
	  (switch 
	   ,value
	   ,@(loop for (val . body) in clauses collect
		  (if (eql val '|t|)
		    `(:default (return-last ,@body))
		    `(,val (return-last ,@body)))))))

(define-javascript-macro |1+| (form)
  `(+ ,form 1))

(define-javascript-macro |1-| (form)
  `(- ,form 1))


;; Equality operators

(define-javascript-macro |eq| (x y) `(=== ,x ,y))

(define-javascript-macro |equal| (x y) `(== ,x ,y))

(define-javascript-macro = (x y) `(== (- ,x 0) (- ,y 0)))

(define-javascript-macro |string=| (x y) `(== (+ "" ,x) (+ "" ,y)))

;; Assignment

(define-javascript-macro |set| (form value) `(com.gigamonkeys.foo.javascript:= ,form ,value))

(define-javascript-macro |debug| (&rest stuff) `(|alert| (+ ,@stuff)))

;;; DOM

(defun attribute-value (thing)
  (typecase thing
    (string thing)
    (number (princ-to-string thing))
    (symbol thing)))

(define-javascript-macro |xml| (body)
  (multiple-value-bind (tag attributes body) (com.gigamonkeys.foo.html::parse-cons-form body)
    `(|let*| ((document (|make-document-object| "" ,(string-upcase tag) |null|)))
	     ,@(loop for (name value) on attributes by #'cddr
		  collect `(|.setAttribute| (@ document |document-element|) ,(string-upcase name) ,(attribute-value value)))
	     ,@(loop for thing in body collect
		    `(|.appendChild| (@ document |document-element|) (|xml-element| (document) ,thing)))
	     document)))

(define-javascript-macro |xml-element| ((document) body)
  (etypecase body
    (string `(|.create-text-node| ,document ,body))
    (number `(|.create-text-node| ,document (+ "" ,body)))
    (symbol `(|if| (|string=| (|typeof| ,body) "string")
		   (|.create-text-node| ,document ,body)
		   ,body))
    (cons 
     (cond
       ((sexp-form-p com.gigamonkeys.foo.html::*html* body)
	(multiple-value-bind (tag attributes body) (com.gigamonkeys.foo.html::parse-cons-form body)
	  `(|let*| ((element (|.create-element| ,document ,(string-upcase tag))))
		   ,@(loop for (name value) on attributes by #'cddr
			collect `(|.setAttribute| element ,(string-upcase name) ,(attribute-value value)))
		   ,@(loop for thing in body collect
			  `(|.appendChild| element (|xml-element| (,document) ,thing)))
		   element)))
       (t body)))))

(define-javascript-macro |html| (body)
  (flet ((attribute-value (thing)
	   (typecase thing
	     (string thing)
	     (number (princ-to-string thing))
	     (symbol thing))))
    (etypecase body
      (string `(|.create-text-node| |document| ,body))
      (number `(|.create-text-node| |document| (+ "" ,body)))
      (symbol `(|if| (|string=| (|typeof| ,body) "string")
		     (|.create-text-node| |document| ,body)
		     ,body))
      (cons 
       (cond
	 ((sexp-form-p com.gigamonkeys.foo.html::*html* body)
	  (multiple-value-bind (tag attributes body) (com.gigamonkeys.foo.html::parse-cons-form body)
	    `(|let*| ((element (|.create-element| |document| ,(string-upcase tag))))
		     ,@(loop for (name value) on attributes by #'cddr
			  collect `(|.set-attribute| element ,(string-upcase name) ,(attribute-value value)))
		     ,@(loop for thing in body collect
			    `(|.append-child| element (|html| ,thing)))
		     element)))
	 (t body))))))
    
	     
