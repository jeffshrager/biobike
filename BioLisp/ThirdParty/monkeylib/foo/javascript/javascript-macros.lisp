;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.javascript)

(defvar *counter* 0)

(defun javascript-gensym (&optional (prefix "g"))
  "Not a proper GENSYM of course since there are no real symbols."
  (intern (format nil "~a~d" prefix (incf *counter*)) :keyword))

(defun intercap->hyphenated (string)
  (with-output-to-string (out)
    (loop for char across string do
       (cond
	 ((upper-case-p char) 
	  (write-char #\- out)
	  (write-char (char-downcase char) out))
	 (t (write-char char out))))))

(define-javascript-macro defun (name (&rest params) &body body)
  `(function ,name ,params ,@body))

(define-javascript-macro defmethod ((class name) (&rest params) &body body)
  `(= (@ ,class |prototype| ,name) (function ,params ,@body)))

(define-javascript-macro defvar (name &optional (value nil value-supplied-p))
  (if value-supplied-p
      `(var ,name ,value)
      `(var ,name)))

(define-javascript-macro debug (&rest params)
  `(|alert| (+ ,@params)))

(define-javascript-macro lambda ((&rest params) &body body)
  `(function ,params ,@body))

(define-javascript-macro let ((&rest bindings) &body body)
  `(prog
     ,@(loop for (var value) in bindings collect `(var ,var ,value))
     ,@body))

;; At the moment there's no difference between let and let* (they're
;; both really like let* (or worse) since neither really introduces a
;; new lexical scope). I provide both so code written to use the right
;; one will be more likely to work if we tighten up the semantics
;; later.
(define-javascript-macro let* ((&rest bindings) &body body)
  `(let ,bindings ,@body))

;; This is technically better except that because of the
;; expression/statement dichotomy it requires you to explicitly
;; (return (let ...)) in addition to the (return ...) within the body
;; of the let.
#+(or)(define-javascript-macro |let| ((&rest bindings) &body body)
	(let ((params (mapcar #'first bindings))
	      (values (mapcar #'second bindings)))
	  `((FUNCTION ,params ,@body) ,@values)))

#+(or)(define-javascript-macro |let*| ((&rest bindings) &body body)
	(if bindings
	    `(|let| (,(first bindings)) (|let*| ,(rest bindings) ,@body))
	    `(PROG ,@body)))

(define-javascript-macro if (condition then &optional else)
  (if else 
      `(if ,condition (block ,then) (block ,else))
      `(if ,condition (block ,then))))

(define-javascript-macro when (condition &body body)
  `(if ,condition (block ,@body)))

(define-javascript-macro unless (condition &body body)
  `(if (! ,condition) (block ,@body)))

(define-javascript-macro cond (&rest clauses)
  (destructuring-bind ((condition &rest body) &rest rest) clauses
    (if (eql condition '|t|)
	`(block ,@body)
	`(if ,condition (block ,@body) ,(when rest `(cond ,@rest))))))

(define-javascript-macro case (value &rest clauses)
  `(switch ,value
    ,@(loop for (case . body) in clauses collect
	   (if (eql case '|t|)
	       `(:default ,@body (break))
	       `(,case ,@body (break))))))

(define-javascript-macro dolist ((var value) &body body)
  `(for ((var ,var) in ,value) (block ,@body)))

(define-javascript-macro dotimes ((var times) &body body)
  `(for ((var (= ,var 0)) (< ,var ,times) (++ ,var :post))
	(block ,@body)))

(define-javascript-macro dokids ((var element) &body body)
  `(for ((var (= ,var (@ ,element |firstChild|))) (!= ,var |null|) (= ,var (@ ,var |nextSibling|)))
       (block ,@body)))

(define-javascript-macro autoref (start &rest indices)
  (if (rest indices)
    (let ((tmp (javascript-gensym "tmp")))
      `((FUNCTION () 
        (let ((,tmp (ref ,start ,(first indices))))
          (when (== ,tmp |null|)
            (= ,tmp (new |Array|))
	    (= (ref ,start ,(first indices)) ,tmp))
	  (return (autoref (ref ,start ,(first indices)) ,@(rest indices)))))))
    `(ref ,start ,(first indices))))

(define-javascript-macro autorefset (value start &rest indices)
  (if (rest indices)
    (let ((tmp (javascript-gensym "tmp")))
      `(let ((,tmp (ref ,start ,(first indices))))
          (when (== ,tmp |null|)
            (= ,tmp (new |Array|))
	    (= (ref ,start ,(first indices)) ,tmp))
	  (autorefset ,value (ref ,start ,(first indices)) ,@(rest indices))))
    `(= (ref ,start ,(first indices)) ,value)))

(define-javascript-macro setf (form value)
  (if (and (consp form) (eql (car form) 'autoref))
    `(autorefset ,value ,@(rest form))
    `(= ,form ,value)))
  

(define-javascript-macro defcallback (name (event &rest params) &body body)
  `(defun ,name (,@params)
     (return
      (lambda (,event) ,@body))))

(define-javascript-macro destructure ((&rest params) obj &body body)
  (let ((gensym (javascript-gensym)))
    `(let ((,gensym ,obj))
       (let (,@(loop for p in params collect `(,p (@ ,gensym ,p)))) ,@body))))

(define-javascript-macro html (form)
  `((function () ,@(tree-builder form))))

(define-javascript-macro define-builder (name (&rest params) tree)
  `(function ,name (,@params) ,@(tree-builder tree)))

(defun find-elements (tree &optional (counter 1))
  (cond
    ((cons-form-p tree)
     (multiple-value-bind (tag attributes body) (parse-cons-form tree)
       (case tag
	 (:text (list `(,counter text ,(second tree))))
	 (:node (list `(,counter code ,(second tree))))
	 (t (cons `(,counter element ,tag ,attributes) (mapcan #'(lambda (x) (find-elements x (1+ counter))) body))))))
    ((consp tree) ;; non FOO cons forms
     (list `(,counter code ,tree)))
    (t
     (list `(,counter text ,tree)))))

(defun tree-builder (tree)
  (flet ((genvar (x c)
	   (intern (format nil "~(~a~)~d" x c) :keyword)))
  (let ((elements (find-elements tree)))
    (loop for counter from 1
       for (depth type value attributes) in elements 
       for var = (genvar (ecase type (element value) (text "text") (code "value")) counter)
       collect `(,depth ,var) into tree-stack
       if (eql type 'element)
       nconc `((var ,var ((@ |document| |createElement|) ,(string-downcase value)))
	       ,@(loop for (k v) on attributes by #'cddr collect
		      `((@ ,var |setAttribute|) ,(string-downcase k) ,v)))
       into code
       else if (eql type 'text)
       nconc `((var ,var ((@ |document| |createTextNode|) ,value))) into code
       else if (eql type 'code)
       nconc `((var ,var ,value)) into code
       finally (return (nconc code (stitch tree-stack)))))))

(defun stitch (vars)
  (loop with stack = ()
     for entry in vars
     do (loop while (and stack (>= (car (car stack)) (car entry))) do (pop stack))
     when stack collect `((@  ,(second (car stack)) |appendChild|) ,(second entry)) into code
     do (push entry stack)
       finally (return (nconc code `((return ,(second (car (last stack)))))))))




      

