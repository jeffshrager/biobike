;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-
;; 
;; Author: JP Massar.

(in-package :bbi)

(defun cw-error-cont (type format-string &rest format-args)
  (declare (ignore type))
  ;;  (setq *code-walk-errors?* t)
  ;; (apply #'cw-compose-message nil type format-string format-args)
  (error (apply 'format nil format-string format-args))
  nil
  )

(defun remove-nils (list) (remove-if 'null list))

(defun initial-lambda-keyword-check (lambda-list)
  (let ((optional-pos (position '&optional lambda-list))
	(rest-pos (position '&rest lambda-list))
	(key-pos (position '&key lambda-list))
	(allow-other-keys-pos (position '&allow-other-keys lambda-list))
	(aux-pos (position '&aux lambda-list))
	(key-out-of-order? nil)
	)
    (flet ((out-of-order (key1 key2)
	     (setq key-out-of-order? t)
	     (cw-error-cont
	      :illegal-syntax
	      "The ~A ~A ~A ~A"
	      key1
	      "lambda list keyword occurs before the"
	      key2
	      "lambda list keyword but they must occur in the opposite order."
	      )))
      (when aux-pos
	(cond
	 ((and allow-other-keys-pos (< aux-pos allow-other-keys-pos))
	  (out-of-order '&aux '&allow-other-keys))
	 ((and key-pos (< aux-pos key-pos))
	  (out-of-order '&aux '&key))
	 ((and rest-pos (< aux-pos rest-pos))
	  (out-of-order '&aux '&rest))
	 ((and optional-pos (< aux-pos optional-pos))
	  (out-of-order '&aux '&optional))
	 ))
      (when allow-other-keys-pos
	(cond
	 ((and key-pos (< allow-other-keys-pos key-pos))
	  (out-of-order '&allow-other-keys '&key))
	 ((and rest-pos (< allow-other-keys-pos rest-pos))
	  (out-of-order '&allow-other-keys '&rest))
	 ((and optional-pos (< allow-other-keys-pos optional-pos))
	  (out-of-order '&allow-other-keys '&optional))
	 ))
      (when key-pos
	(cond
	 ((and rest-pos (< key-pos rest-pos))
	  (out-of-order '&key '&rest))
	 ((and optional-pos (< key-pos optional-pos))
	  (out-of-order '&key '&optional))
	 ))
      (when rest-pos
	(cond
	 ((and optional-pos (< rest-pos optional-pos))
	  (out-of-order '&rest '&optional))
	 ))
      (values 
       optional-pos rest-pos key-pos 
       allow-other-keys-pos aux-pos key-out-of-order?)
      )))

(defun verify-lambda-list-symbol (arg what)
  (cond
   ((not (symbolp arg))
    (cw-error-cont
     :illegal-syntax
     "Lambda list ~A, ~A, is not a symbol" what arg))
   ((constantp arg)
    (cw-error-cont
     :illegal-semantics
     "Lambda list ~A, ~A, has been proclaimed constant" what arg))
   (t arg)
   ))

;;; Returns three values:
;;;   -- A list of valid parameter names
;;;   -- A list of the binding forms for each valid parameter name
;;;   -- Whether any problem with syntax or semantics was detected

(defun verify-aux-lambda-arguments (auxlist)
  (let* ((valid-bindings 
	  (remove 
	   nil
	   (mapcar 
	    #'(lambda (aux) (canonicalize-let-binding aux '&aux)) auxlist)))
	 (bound-symbols (mapcar #'car valid-bindings)))
    (values 
     (mapcar #'car valid-bindings)
     valid-bindings
     (= (length auxlist) (length bound-symbols)))
    ))
  
;;; Returns two values 
;;;   -- the valid &rest parameter name or NIL if it is invalid.
;;;   -- whether the parse was successful

(defun verify-rest-lambda-arguments (rest-list)
  (flet ((verify-&rest-param (p)
	   (if (verify-lambda-list-symbol p "&rest parameter") 
	       (values p t)
	     (values nil nil))))
    (let ((len (length rest-list)))
      (cond
       ((zerop len)
	(cw-error-cont
	 :illegal-syntax
	 "Lambda list &rest key requires a variable name")
	(values nil nil))
       ((= len 1) (verify-&rest-param (first rest-list)))
       (t
	(cw-error-cont
	 :illegal-syntax
	 "Lambda list &rest key requires exactly one variable, got ~S (~S)"
	 (length rest-list) rest-list
	 )
	(multiple-value-bind (param ok?)
	    (verify-&rest-param (first rest-list))
	  (declare (ignore ok?))
	  (values param nil)
	  ))))))

;;; Returns two values
;;;   -- A list of valid required parameters
;;;   -- Whether any problem with syntax or semantics was detected

(defun verify-required-lambda-arguments (required-args)
  (let ((valid-args
	 (remove
	  nil
	  (mapcar
	   #'(lambda (arg) (verify-lambda-list-symbol arg "required argument"))
	   required-args
	   ))))
    (values valid-args (= (length required-args) (length valid-args)))
    ))

;;; Returns four values:
;;;   -- A list of valid parameter names
;;;   -- A list of the binding forms for each parameter name
;;;   -- Whether any problem with syntax or semantics was detected
;;;   -- A list of SUPPLIED-P parameter names to go with each
;;;      valid parameter name, or NIL for a parameter name with
;;;      no SUPPLIED-P parameter name.

(defun verify-optional-lambda-arguments (optional-list)
  (let ((results nil))
    (dolist (optarg optional-list)
      (when (symbolp optarg) (setq optarg (list optarg nil)))
      (if (not (listp optarg))
	  (cw-error-cont
	   :illegal-syntax
	   "Argument list &optional parameter, ~A, is invalid" optarg)
	(let ((len (length optarg)) (supplied-p-ok? t))
	  (when (> len 3)
	    (cw-error-cont
	     :illegal-syntax
	     "Argument list &optional parameter, ~S, is not of the form ~A"
	     optarg
	     "(<var> <init-form> <supplied-p-parameter>)"
	     ))
	  (when (>= len 3)
	    (setq supplied-p-ok?
	      (verify-lambda-list-symbol 
	       (third optarg) "&optional supplied-p parameter")))
	  (when (and (verify-lambda-list-symbol 
		      (first optarg) "&optional parameter")
		     supplied-p-ok?)
	    (push optarg results))
	  )))
    (setq results (nreverse results))
    (values
     (mapcar #'car results)
     (mapcar #'(lambda (x) (list (first x) (second x))) results)
     (= (length results) (length optional-list))
     (mapcar #'third results)
     )))

;;; Returns five values:
;;;   -- A list of valid parameter names
;;;   -- A list of the binding forms for each parameter name
;;;   -- Whether any problem with syntax or semantics was detected
;;;   -- A list of SUPPLIED-P parameter names to go with each
;;;      valid parameter name, or NIL for a parameter name with
;;;      no SUPPLIED-P parameter name.
;;;   -- A list of the KEYWORD-NAMEs for each valid parameter name.
;;;      If no KEYWORD-NAME was provided, the parameter name as a
;;;      keyword is returned.

(defun verify-key-lambda-arguments (key-list)
  (let ((results nil))
    (dolist (keyarg key-list)
      (when (symbolp keyarg) (setq keyarg (list keyarg nil)))
      (if (not (listp keyarg))
	  (cw-error-cont
	   :illegal-syntax
	   "Argument list &key parameter, ~A, is invalid" keyarg)
	(let ((len (length keyarg)) (supplied-p-ok? t))
	  (when (> len 3)
	    (cw-error-cont
	     :illegal-syntax
	     "Argument list &key parameter, ~S, is not of the form ~A"
	     keyarg
	     "(<keyform> <init-form> <supplied-p-parameter>)"
	     ))
	  (when (>= len 3)
	    (setq supplied-p-ok?
	      (verify-lambda-list-symbol 
	       (third keyarg) "&key supplied-p parameter")))
	  (let ((keyword-name-form (first keyarg)))
	    (when (symbolp keyword-name-form)
	      (setq keyword-name-form
		(list (intern (symbol-name keyword-name-form) :keyword)
		      keyword-name-form
		      )))
	    (cond
	     ((not (listp keyword-name-form)) 
	      (cw-error-cont
	       :illegal-syntax
	       "Argument list &key parameter name, ~S, is invalid"
	       keyword-name-form
	       ))
	     ((or (not (= (length keyword-name-form) 2))
		  (not (symbolp (first keyword-name-form)))
		  (not (symbolp (second keyword-name-form))))
	      (cw-error-cont
	       :illegal-syntax
	       "Argument list &key parameter name, ~S, is not of the form ~A"
	       keyword-name-form
	       "(<keyword-name> <var>)"
	       ))
	     ((not (verify-lambda-list-symbol 
		    (second keyword-name-form) "&key parameter"))
	      nil
	      )
	     (t 
	      (when supplied-p-ok?
		(push (cons keyword-name-form (rest keyarg)) results)))
	     )))))
    ;; (ppdbg keyarg results))
    (setq results (nreverse results))
    (values
     (mapcar #'cadar results)
     (mapcar #'(lambda (x) (list (cadar x) (second x))) results)
     (= (length results) (length key-list))
     (mapcar #'third results)
     (mapcar #'caar results)
     )))
	     

(defun separate-argument-lists 
    (lambda-list opos rpos kpos allowpos auxpos)
  (let ((rlist (reverse lambda-list))
	(len (length lambda-list))
	(current-pos 0)
	(reqlist nil) (optlist nil)
	(restlist nil)(keylist nil) (auxlist nil)
	)
    (flet ((reverse-pos (p) (and p (- len (1+ p)))))
      (macrolet ((rev (x) `(setq ,x (nreverse ,x))))
	(let ((ropos (reverse-pos opos))
	      (rrpos (reverse-pos rpos))
	      (rkpos (reverse-pos kpos))
	      (rallowpos (reverse-pos allowpos))
	      (rapos (reverse-pos auxpos))
	      )
	  (when rapos 
	    (setq auxlist (subseq rlist current-pos rapos))
	    (setq current-pos (1+ rapos)))
	  (when rallowpos (setq current-pos (1+ rallowpos)))
	  (when rkpos
	    (setq keylist (subseq rlist current-pos rkpos))
	    (setq current-pos (1+ rkpos)))
	  (when rrpos
	    (setq restlist (subseq rlist current-pos rrpos))
	    (setq current-pos (1+ rrpos)))
	  (when ropos
	    (setq optlist (subseq rlist current-pos ropos))
	    (setq current-pos (1+ ropos)))
	  (setq reqlist (subseq rlist current-pos))
	  (values
	   (rev reqlist) (rev optlist)
	   (rev restlist) (rev keylist) (rev auxlist)
	   ))))))

(defun walk-standard-lambda-list (lambda-list decl-frame)
  (declare (ignore decl-frame))
  (block exit

    ;; Find out which lambda list keywords are present and
    ;; where they are.  If any are out of order we give up.

    (multiple-value-bind (opos rpos kpos allowpos auxpos out-of-order?)
	(initial-lambda-keyword-check lambda-list)
      (when out-of-order? (return-from exit (values nil nil)))
      ;; Is &allow-other-keys out of position?
      (when (or (and allowpos (not kpos))
		(and allowpos kpos (< allowpos kpos))
		(and allowpos auxpos (> allowpos auxpos))
		)
	(cw-error-cont
	 :illegal-syntax
	 "Lambda list keyword &allow-other-keys must follow &key")
	(return-from exit (values nil nil))
	)

      ;; Get all the forms associated with each lambda keyword

      (let (reqlist olist rlist klist auxlist
	    reqvars reqok? 
	    optvars optbindings optok? opt-supplied-p-vars
	    restvar restok?
	    keyvars keybindings keyok? key-supplied-p-vars keynames
	    auxvars auxbindings auxok?
	    bound-variables init-forms
	    )
	(declare (ignore reqok? optok? restok? keyok? auxok?))
	(declare (ignore keynames))
	(multiple-value-setq (reqlist olist rlist klist auxlist)
	  (separate-argument-lists 
	   lambda-list opos rpos kpos allowpos auxpos))

	;; For every lambda keyword, check all the forms associated
	;; with it, returning appropriate information about the
	;; bound variable names, the init forms, etc.

	(when reqlist
	  (multiple-value-setq (reqvars reqok?)
	    (verify-required-lambda-arguments reqlist)))
	(when olist
	  (multiple-value-setq (optvars optbindings optok? opt-supplied-p-vars)
	    (verify-optional-lambda-arguments olist)))
	(when rlist
	  (multiple-value-setq (restvar restok?)
	    (verify-rest-lambda-arguments rlist)))
	(when klist
	  (multiple-value-setq 
	      (keyvars keybindings keyok? key-supplied-p-vars keynames)
	    (verify-key-lambda-arguments klist)))
	(when auxlist
	  (multiple-value-setq (auxvars auxbindings auxok?)
	    (verify-aux-lambda-arguments auxlist)))

	;; Create the list of all the variables to be bound by
	;; this lambda list, in left-to-right order.

	(setq bound-variables
	  (append
	   (and reqlist reqvars)
	   (and olist optvars)
	   (and olist (remove-nils opt-supplied-p-vars))
	   (and rlist restvar (list restvar))
	   (and klist keyvars)
	   (and klist (remove-nils key-supplied-p-vars))
	   (and auxlist auxvars)
	   ))

	;; Create the list of all the init-forms, in one-to-one
	;; correspondence with the list of bound variables.  If
	;; any bound variable does not have an init-form, the 
	;; INIT-FORM will be NIL.  Each INIT-FORM is made into a
	;; list, so as to distinguish a NIL INIT-FORM from a
	;; non-existent one.

	(flet ((always-nil (x) (declare (ignore x)) nil))
	  (setq init-forms
	    (append
	     (and reqlist (mapcar #'always-nil reqvars))
	     (and olist (mapcar #'list (mapcar #'second optbindings)))
	     (and olist 
		  (mapcar #'always-nil (remove-nils opt-supplied-p-vars)))
	     (and rlist (list nil))
	     (and klist (mapcar #'list (mapcar #'second keybindings)))
	     (and klist 
		  (mapcar #'always-nil (remove-nils key-supplied-p-vars)))
	     (and auxlist (mapcar #'list (mapcar #'second auxbindings)))
	     )))
	   
	(values bound-variables init-forms)

	))))
      
	
(defun canonicalize-let-binding (binding &optional (what 'let))
  (when (symbolp binding) (setq binding (list binding nil)))
  (flet ((verify-non-constant-binding (binding)
	   (if (constantp (first binding))
	       (cw-error-cont
		:illegal-semantics
		"~A binding for ~S has a bound variable that is a constant" 
		what binding)
	     binding)))
    (cond
     ((not (listp binding))
      (cw-error-cont
       :illegal-syntax 
       "~A binding form, ~S, is neither a symbol nor a list" what binding))
     ((not (symbolp (first binding)))
      (cw-error-cont
       :illegal-syntax 
       "~A binding form ~S has non-symbol as <var>" what binding))
     ((> (length binding) 2)
      (cw-error-cont
       :illegal-syntax 
       "~A binding form, ~S, is not of the form (<var> <init>)" what binding)
      (verify-non-constant-binding binding)
      nil
      )
     (t (verify-non-constant-binding binding))
     )))
	

	  

      