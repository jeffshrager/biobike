;;; -*- package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

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


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-macro-user-symbols* 
    '(
      vif 
      vwhen
      vwhile
      vcond
      mvsetf 
      assocadr
      assocdr
      ))

  (defparameter *utility-macro-api-symbols*
    (append 
     *utility-macro-user-symbols* 
     '(
      ;; defined in string-utils.lisp for compilation order considerations
       optimization-declaration
       lsetq
       mvb
       mvsetf
       define-url&args
       multiple-continuable-errors
       with-multiple-continuable-errors
       ppme
       with-destructured-keys
       )))

  (export *utility-macro-api-symbols* (find-package :utils)))

;;; LSETQ gives the length instead of the value, and is good for
;;; setting long strings.

;;; This is the original version in the first lesson:

; (defmacro lsetq (var value)
;  `(length (setq ,var ,value)))


;;; This is a better version.
;;; It gives you back both the type of the object, and the length.
;;; If it can't figure out the length, you get the type and the
;;; object itself.

(defun dwim-lsetq-func (x)
  (cond ((listp x) (list 'list (length x)))
        ((stringp x) (list 'string (length x)))
        ((vectorp x) (list 'vector (length x)))
        (t (list (type-of x) x))))

(defmacro lsetq (var value)
  #.(one-string
     "Variation of SETQ which returns length or type of value, "
     "instead of value itself")
   `(dwim-lsetq-func (setq ,var ,value)))

;;; These macros bind the variable it to be the result of some test so
;;; that their bodies may use anaphora for the test value
#+problem-with-export-of-it
(defmacro aif (test then &optional else)
  "Like IF, but lexically binds the symbol IT to the result of TEST"
  `(let ((it ,test))
     (if it ,then ,else)))

#+problem-with-export-of-it
(defmacro awhen (test &body body)
  "Like WHEN, but lexically binds the symbol IT to the result of TEST"
  `(aif ,test (progn ,@body)))

#+problem-with-export-of-it
(defmacro awhile (expr &body body)
  #.(one-string-nl
     "Executes BODY as long as EXPR is non-nil, lexically binding the"
     "the symbol IT to the value of EXPR on each iteration step.")
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro vif ((var test) then &optional (else nil))
  "Like IF, but lexically binds VAR to result of TEST in scope of THEN/ELSE"
  (unless (symbolp var) 
    (error "VIF wants a symbol as the first element of the test clause!"))
  `(let ((,var ,test)) (if ,var ,then ,else)))
(defmacro vwhen ((var test) &body body)
  "Like WHEN, but lexically binds VAR to result of TEST in scope of body"
  (unless (symbolp var) 
    (error "VWHEN wants a symbol as the first element of the test clause!"))
  `(let ((,var ,test)) (when ,var ,@body)))
(defmacro vwhile ((var expr &rest end-forms) &body body)
  #.(one-string-nl
     "Executes BODY as long as EXPR is non-nil, lexically binding the"
     "the symbol VAR to the value of EXPR on each iteration step."
     "END-FORMS are executed when EXPR returns NIL and VWHILE returns the"
     "value of evaluating the last END-FORM.")
  (unless (symbolp var) 
    (error "VWHILE wants a symbol as the first element of expression form!"))
  `(do ((,var ,expr ,expr)) ((not ,var) ,@end-forms) ,@body))

(defmacro vcond (&body vcond-clauses)
  #.(one-string-nl
     "Binds a variable to the result of each conditional for"
     "the scope of the clauses executed if that conditional is true."
     "Instead of a simple conditional as the first element of a COND"
     "clause, VCOND takes a 2-element form, (symbol conditional).")
  ;; check syntax
  (loop for vc in vcond-clauses do
        (cond
         ((not (consp vc)) (error "Illegal VCOND clause: ~S" vc))
         ((not (consp (first vc))) nil)
         ((not (symbolp (caar vc))) (error "Illegal VCOND clause: ~S" vc))
         (t nil)
         ))
  (when vcond-clauses
    (let ((vc (first vcond-clauses)))
      (cond
       ((not (consp (first vc))) 
        `(if ,(first vc) (progn ,@(rest vc)) (vcond ,@(rest vcond-clauses))))
       ((symbolp (caar vc))
        ;; make sure variable is only bound for duration of clause!
        (let ((vcond-clause-symbol (gensym "VC-")))
          `(vif (,vcond-clause-symbol ,(cadar vc))
                (let ((,(caar vc) ,vcond-clause-symbol)) ,@(rest vc))
                (vcond ,@(rest vcond-clauses))
                )))))))
  
(defmacro assocadr (key alist &rest assoc-keywords)
  "Shorthand for (CADR (ASSOC ...))"
  `(cadr (assoc ,key ,alist ,@assoc-keywords)))

(defmacro assocdr (key alist &rest assoc-keywords)
  "Shorthand for (CDR (ASSOC ...))"
  `(cdr (assoc ,key ,alist ,@assoc-keywords)))

;;; Simplifactions for multiple-value-bind

(defmacro mvb (vars expr &rest body)
  "Shorthand for MULTIPLE-VALUE-BIND"
  `(multiple-value-bind ,vars ,expr ,@body))

;;; This is a generalized multiple value setf.

(defmacro mvsetf (setf-exprs value-expr)
  #.(one-string-nl
     "A generalization of MULTIPLE-VALUE-SETQ.  The SETF-EXPRS can be "
     "any form which is a valid SETF place.")
  (let ((temp+setf-expr 
	 (loop for expr in setf-exprs
	       collect (cons (gensym "TEMP-") expr))))
    `(multiple-value-bind
	 ,(mapcar #'car temp+setf-expr)
	 ,value-expr
         ,@(loop for (temp . expr) in temp+setf-expr 
              collect `(setf ,expr ,temp)))))

;; Execute the body LIMIT times, then the next time print out LIMIT-MESSAGE,
;; and after that exits.  The BODY is anything after the first 'DO
;; found in DO-CONTROL-AND-BODY

;; Example:
;; (limited-error-message-loop 3 t for j from 0 below 100 do (print j))
;; 0
;; 1
;; 2
;; ... and more errors.
;; NIL

(defmacro limited-error-message-loop 
          (limit limit-message &rest do-control-and-body)
  "Too complicated to explain in a few lines.  Not exported."
  (let ((do-pos (position 'do do-control-and-body))
        (count-var (gensym "COUNT-"))
        (limit-var (gensym "LIMIT-")))
    (cond
     ((or (eq limit-message t) (eq limit-message nil))
      (setq limit-message "... and more errors."))
     ((not (stringp limit-message))
      (error "LIMITED-ERROR-MESSAGE-LOOP: Message must be literal string."))
     (t nil))
    (unless do-pos
      (error "LIMITED-ERROR-MESSAGE-LOOP: There must be a DO before the body"))
    (let ((loop-controls (subseq do-control-and-body 0 (1+ do-pos)))
          (body-forms (subseq do-control-and-body (1+ do-pos))))
      `(let ((,limit-var ,limit))
         (loop for ,count-var from 0 ,@loop-controls
               (cond
                ((< ,count-var ,limit-var) ,@body-forms)
                ((= ,count-var ,limit-var) (cformatt ,limit-message))
                (t (return))
                ))))))


(defun url-with-parameters (url &rest parameter-snippets)
  (one-string url "?" (string-join parameter-snippets "&")))

(defmacro define-url&args (name path &rest args)
  #.(one-string-nl
     "Defines a variable, whose name is NAME surrounded by '*', and whose"
     "value is PATH."
     "Defines another variable, whose name is NAME prepended with '*' and"
     "postpended with '-TEMPLATE*', and whose value is a format string which"
     "is a template for the URL."
     "Defines a function, whose name is NAME prepended with 'MAKE-', which"
     "takes keyword arguments.  This function calls FORMAT using the format"
     "string variable defined above with the keyword argument values being"
     "the format arguments. It returns a URL string of the form:"
     "PATH?<keyname1>=<keyval1>&<keyname2>=<keyval2>..."
     "All the newly created symbols are created in the same package as NAME.")
  (let* ((defaults (list "~A" "nil"))
         (intern-package (symbol-package name))
         (canonicalized-args
          (loop for arg in args collect
                (cond
                 ((not (listp arg)) (cons arg defaults))
                 (t 
                  (cond
                   ((= (length arg) 1) (append arg defaults))
                   ((= (length arg) 2) (append arg (cdr defaults)))
                   (t arg)
                   )))))
         (argstrings
          (loop for (arg format) in canonicalized-args collect
                (one-string (string arg) "=" format)))
         (argkeys
          (loop for (arg format default) in canonicalized-args collect
                (progn format (list (intern (string arg)) default))))
         (url-variable 
          (intern (one-string "*" (string name) "*") intern-package))
         (url-template-variable
          (intern (one-string "*" (string name) "-TEMPLATE*") intern-package))
         (url-construction-function-name 
          (intern (one-string "MAKE-" (string name)) intern-package))
         )
    `(progn
       (defparameter ,url-variable ,path)
       (defparameter ,url-template-variable
         (url-with-parameters ,url-variable ,@argstrings))
       (defun ,url-construction-function-name (&key ,@argkeys)
         (funcall 'format nil ,url-template-variable ,@(mapcar 'first argkeys))
         ))))



(define-condition multiple-continuable-errors (error) 
  ((error-list :initarg :error-list :accessor mce-error-list)
   (info-string :initarg :info-string :accessor mce-info-string) 
   (print-count :initarg :print-count :accessor mce-print-count))
  (:report
   (lambda (condition stream)
     (let* ((print-count (mce-print-count condition))
            (error-list (mce-error-list condition))
            (nerrors (length error-list)))
       (format stream "*** ~D error~P occurred ~A~%"
               nerrors nerrors (mce-info-string condition))
       (terpri stream)
       (loop for error in error-list
             for j from 0 below print-count
             as error-string = (format nil "~A" error)
             do 
             (format 
              stream "~A~%~%" 
              (precede-with-header-and-indent error-string nil)))
       (when (> nerrors print-count)
         (format stream "~%  and ~D more error~P ...~%" 
                 (- nerrors print-count) (- nerrors print-count)))
       ))))
              

(defmacro with-multiple-continuable-errors
          ((maximum-error-count 
            n-errors-to-print 
            restart-name 
            info-string
            &key (errors-to-catch '(error)))
           &body body)
  #.(one-string-nl 
     "Executes BODY in the context of error handlers for ERRORS-TO-CATCH"
     "type errors. "
     "If no such errors occur in BODY the form returns normally."
     "When such an error occurs, if a restart named RESTART-NAME exists"
     "the restart is called after saving the error."
     "If MAXIMUM-ERROR-COUNT such errors occur, a MULTIPLE-CONTINUABLE-ERRORS"
     "error is signalled (which contains information about all"
     "the errors that occurred)."
     "If at least one but less than MAXIMUM-ERROR-COUNT such errors occur"
     "and restarts are found for all of them, then when BODY finishes"
     "executing a MULTIPLE-CONTINUABLE-ERRORS error is signalled."
     "N-ERRORS-TO-PRINT is passed to the MULTIPLE-CONTINUABLE-ERRORS"
     "condition and tells it how many of the errors to actually display."
     "INFO-STRING is also passed likewise and is printed out in the"
     "error header.")
  (let ((error-list (gensym "ERROR-LIST-"))
        (multi-error-conser (gensym "MULTI-ERROR-CONSER-"))
        (result (gensym "RETURN-"))
        (ok-label (gensym "OK-"))
        (oops-label (gensym "OOPS-")))
    `(block ,ok-label
       (let ((,error-list nil))
         (error
          (block ,oops-label
            (flet ((,multi-error-conser ()
                     (return-from ,oops-label
                       (make-condition 
                        'multiple-continuable-errors
                        :error-list (reverse ,error-list)
                        :info-string ,info-string
                        :print-count ,n-errors-to-print
                        ))))
              (handler-bind 
                  (((or ,@errors-to-catch)
                    (lambda (c) 
                      (push c ,error-list)
                      (when (>= (length ,error-list) ,maximum-error-count)
                        (,multi-error-conser))
                      (let ((restart-object (find-restart ',restart-name)))
                        (if (null restart-object)
                            (,multi-error-conser)
                          (invoke-restart restart-object)
                          )))))
                (let ((,result (progn ,@body)))
                  (if ,error-list 
                      (,multi-error-conser)
                    (return-from ,ok-label ,result)
                    ))))))))))

(defmacro ppme (form &optional (use-macroexpand? nil))
  #.(one-string-nl
     "Macroexpands and then pretty prints FORM.  A useful abbreviation."
     "If USE-MACROEXPAND? is T, then MACROEXPAND is used rather than"
     "MACROEXPAND-1.")
  (if use-macroexpand? 
      `(pprint (macroexpand ',form)))
  `(pprint (macroexpand-1 ',form)))

(defun remove-key-and-value (key keylist)
  (cond 
   ((null keylist) nil)
   ((eq key (first keylist)) (cddr keylist))
   (t (cons (first keylist) 
            (cons (second keylist)
                  (remove-key-and-value key (cddr keylist))
                  )))))
   

(defmacro with-destructured-keys 
          (((&rest required-vars) (&rest named-key-vars) other-args-var)
           arglist 
           &body body)
  #.(one-string-nl
     "Destructures an argument list, ARGLIST, with some number"
     "of required arguments"
     "and some number of explicit keywords, and an arbitrary number of"
     "unknown keywords.  The list of unknown keywords and their values"
     "gets bound to OTHER-ARGS-VAR, while the required args and named"
     "keywords get bound in the usual manner."
     "The body is then executed with the bindings in effect.")
  (multiple-value-bind (doc decls real-body)
      (parse-doc-decls-body body)
    (when doc (error "No docstring allowed!"))
    `(destructuring-bind 
         (,@required-vars 
          &rest ,other-args-var &key ,@named-key-vars &allow-other-keys)
         ,arglist
       ,@decls
       (loop for named-key in ',(mapcar 'keywordize named-key-vars) do
             (setq ,other-args-var
                   (remove-key-and-value named-key ,other-args-var)))
       ,@real-body
       )))

