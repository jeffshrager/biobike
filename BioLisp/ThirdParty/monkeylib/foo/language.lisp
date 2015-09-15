;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.language)

;;; Hmmmm. Might be useful to support symbol macros.

(defclass language ()
  ((special-operator-symbol
    :initarg :special-operator-symbol
    :accessor special-operator-symbol)
   (macro-symbol
    :initarg :macro-symbol
    :accessor macro-symbol)
   (input-readtable
    :initarg :input-readtable
    :accessor input-readtable)
   (input-package
    :initarg :input-package
    :accessor input-package)
   (output-file-type
    :initarg :output-file-type
    :accessor output-file-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primary interface

(defgeneric special-operator-symbol (language)
  (:documentation "Return the symbol added to a symbol's plist to
  indicate it is the name of a special operator in LANGUAGE."))

(defgeneric macro-symbol (language)
  (:documentation "Return the symbol added to a symbol's plist to
  indicate it has been defined as a macro in LANGUAGE."))

(defgeneric identifier (language form)
  (:documentation "Extract a symbol that identifies the form."))

(defgeneric sexp-form-p (language form)
  (:documentation "Is the given form a meaningful non-special,
  non-macro form in language."))

(defgeneric embeddable-value-form (language form)
  (:documentation "Return a form that will evaluate to a string
  that can be embedded in the generated output."))

(defgeneric process-sexp (language processor form environment)
  (:documentation "The basic evaluation rule for the language,
  after special operators and macro forms have been handled."))

;;; Secondary interface -- these are typically implemented in terms of
;;; the primary interface.

(defgeneric special-form-p (language form)
  (:documentation "Is the given form a special form in language."))

(defgeneric macro-form-p (language form)
  (:documentation "Is the given form a macro form in language."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler interface

(defgeneric comment (language text)
  (:documentation "Return text as a comment."))

(defgeneric input-readtable (language)
  (:documentation "The readtable we should use to read the input file."))

(defgeneric input-package (language)
  (:documentation "The package we should use to read the input file."))

(defgeneric top-level-environment (language)
  (:documentation "Environment for evaluating top-level forms."))

(defgeneric output-file-type (language)
  (:documentation "File suffix for generated files."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Null implementation of processor interface -- this is used for
;;; walking fors without generating any output.

(defmethod raw-string ((pp (eql nil)) string &optional newlines-p)
  (declare (ignore string newlines-p)))

(defmethod newline ((pp (eql nil))))

(defmethod freshline ((pp (eql nil))))

(defmethod indent ((pp (eql nil))))

(defmethod unindent ((pp (eql nil))))

(defmethod toggle-indenting ((pp (eql nil))))

(defmethod embed-value ((pp (eql nil)) value)
  (declare (ignore value)))

(defmethod embed-code ((pp (eql nil)) code)
  (declare (ignore code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language engine.

(defun process (language processor form environment)
  "Process FORM as an expression in LANGUAGE. The ENVIRONMENT is
provided to special forms and to the basic evaluation rule
implemented by a method on PROCESS-SEXP."
  (cond
    ((special-form-p language form) (process-special-form language processor form environment))
    ((macro-form-p language form)   (process language processor (expand-macro-form language form environment) environment))
    ((sexp-form-p language form)    (process-sexp language processor form environment))
    ((consp form)                   (embed-code processor form))
    (t                              (embed-value processor (embeddable-value-form language form)))))

(defgeneric process-special-form (language processor form environment))

(defgeneric expand-macro-form (language form environment))

(defmethod process-special-form (language processor form environment)
  (let ((special-operator (get (identifier language form) (special-operator-symbol language))))
    (funcall special-operator processor form environment)))

#+(or)(defmethod expand-macro-form :before ((language t) form environment)
  (format t "Expanding~&~s~%in environment ~s~%" form environment))

(defmethod expand-macro-form (language form environment)
  (let ((macro-function (get (identifier language form) (macro-symbol language))))
    (funcall macro-function form environment)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default methods.

(defmethod identifier ((language t) (form cons))
  "Reasonable default for languages with a Lispy syntax."
  (and (symbolp (car form)) (car form)))

(defmethod identifier ((language t) (form t))
  (error "Malformed expression for ~a: ~s" language form))

(defmethod special-form-p ((language t) (form t)) nil)

(defmethod special-form-p ((language t) (form cons))
  (let ((identifier (identifier language form)))
    (and identifier
	 (get identifier (special-operator-symbol language)))))

(defmethod macro-form-p ((language t) (form t)) nil)

(defmethod macro-form-p ((language t) (form cons))
  (let ((identifier (identifier language form)))
    (and identifier
	 (get identifier (macro-symbol language)))))

(defmethod sexp-form-p ((language t) form)
  "Suitable default for languages in which all forms that are not
  special or macros have some meaning. Languages that allow
  embedded code and embedded values will need their own
  specialization of this method."
  (declare (ignore form))
  t)

(defmethod embeddable-value-form ((language t) form)
  "Reasonable default. Languages that need to escape certain
characters will need their own specializations of this method."
  `(princ-to-string ,form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros -- typically specific languages will provide their own
;;; definitional macros that will expand into these two macros.

(defmacro define-special-operator (name special-operator-symbol (processor &rest other-parameters) &body body)
  (with-gensyms (whole)
    (multiple-value-bind (parameters environment) (parse-&environment other-parameters)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name ',special-operator-symbol)
	       (lambda (,processor ,whole ,environment)
		 (declare (ignorable ,environment))
		 (handler-case
		     (destructuring-bind (,@parameters) (rest ,whole)
		       ,@body)
		   (error (e)
		     (error 'foo-syntax-error :form ,whole :cause e)))))))))

(define-condition foo-syntax-error () 
  ((form :initarg :form :accessor form-of)
   (cause :initarg :cause :accessor cause-of :initform nil)))

(defmethod print-object ((c foo-syntax-error) stream)
  (print-unreadable-object (c stream)
    (format stream "in form: ~s; caused by: ~a" (form-of c) (cause-of c))))

(defmacro define-macro (name macro-symbol (&rest parameters) &body body)
  (with-gensyms (whole namevar)
    (multiple-value-bind (parameters environment) (parse-&environment parameters)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name ',macro-symbol)
	       (lambda (,whole ,environment)
		 (handler-case
		     (destructuring-bind (,@(normalize-macro-lambda-list parameters namevar)) ,whole
		       (declare (ignore ,namevar))
		       (declare (ignorable ,environment))
		       ,@body)
		   (error (e)
		     (error 'foo-syntax-error :form ,whole :cause e)))))))))

(defun parse-&environment (parameters)
  "Parse out an optional &environment parameter and return the
parameter list without it and the name of the parameter."
  (let ((cons (member '&environment parameters)))
    (if cons
     (values
      (nconc (ldiff parameters cons) (cddr cons))
      (cadr cons))
     (values parameters (make-symbol (symbol-name '&environment))))))

(defun normalize-macro-lambda-list (parameters namevar)
  "Create a destructuring-lambda list that can parse a whole
macro form, including an aptional &whole parameter and a
parameter to eat up the macro name."
  (let* ((back (if (eql (car parameters) '&whole) (cddr parameters) parameters))
	 (front (ldiff parameters back)))
    `(,@front ,namevar ,@back)))

(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))

(defun sexp->ops (language body environment)
  (loop with compiler = (make-instance 'text-compiler)
     for form in body do (process language compiler form environment)
     finally (return (ops compiler))))

(defun emit (language body environment)
  (process language (get-pretty-printer) body environment))

(defun compile-special-op-body (processor body)
  "Code generator generator."
  (loop for thing in body collect
       (etypecase thing
	 (string `(raw-string ,processor ,thing ,(not (not (find #\Newline thing)))))
	 (cons thing)
	 (keyword
	  (ecase thing
	    (:newline `(newline ,processor))
	    (:freshline `(freshline ,processor))
	    (:indent `(indent ,processor))
	    (:unindent `(unindent ,processor)))))))