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

;;; We need to specify the exact order of arguments for the
;;; call to the auxiliary function (and the auxiliary function definition):

;;; First, all the required arguments (as Lisp required arguments) 
;;; Second, all the token arguments (as Lisp required arguments)
;;; Third, all the flag arguments (as Lisp required arguments)
;;; Fourth, all the keyword arguments (as Lisp keyword arguments)

;;; In a call, therefore, all token arguments and flag arguments
;;; will be provided either as T or NIL.  In a call, a particular
;;; keyword argument may or may not be present.  

;;; Example: 

#||
(define-function foo 
  required (a (b c) d)
  flag (x y)
  keyword fred = 3)
||#

;;; The bbl code (foo 10 b 20 x fred 4) will generate a call to the 
;;; auxiliary function like so:
 
;;; (DF-FUNCTION-FOR-FOO 
;;;    10 20   ; these are the two required arguments
;;;    T       ; this is the value for the token variable B which is present 
;;;    NIL     ; this is the value for the token variable C which isn't present
;;;    T       ; this is the value for the flag variable X which is present
;;;    NIL     ; this is the value for the flag variable Y which isn't present
;;;    :fred 4 ; the keyword fred transformed to a lisp keyword with value
;;;    )

(define-condition df-execute-error (error) 
  ((explanation :initarg :explanation :reader explanation))
  (:report
   (lambda (condition stream)
     (format stream "~A" (explanation condition))
     )))

(defun dfee (format-string &rest format-args)
  (error
   (make-condition
    'df-execute-error
    :explanation (apply 'format nil format-string format-args)
    )))

(defvar *df-source-pathname* nil)

(defun record-df-source-file (name)
  (let ((path 
         (cond
          ((or (stringp *df-source-pathname*)
               (pathnamep *df-source-pathname*))
           (pathname *df-source-pathname*))
          ((or (stringp *load-pathname*) 
               (pathnamep *load-pathname*))
           (pathname *load-pathname*))
          (t :toplevel)
          )))
    (when (pathnamep path) 
      (setq path (maybe-convert-fasl path)))
    (setf (get name :df-source-file) path)
    #+:allegro
    (setf (excl:source-file name :operator) path)
    ))

(defun maybe-convert-fasl (pathname)
  (let ((type (pathname-type pathname)))
    (if (and (stringp type)
             (string-equal type #+:allegro "fasl" #+:lispworks "fsl"))
        (cl-user::change-file-type pathname "lisp")
      pathname
      )))

(defun generate-define-function-forms (df-clauses name &KEY expandable mv) ; *****
  (when (and (fboundp name) 
             (eq (find-package :common-lisp) (symbol-package name)))
    (error 
     (one-string-nl
      "You cannot redefine ~A because it is a Common Lisp function!" 
      "Please use a different name if at all possible, or"
      "contact the system administrators if you really need to do this.")
     name
     ))
  (vif (defun-form (find-df-clause-data :defun df-clauses))
       (generate-defun-and-aliases 
        defun-form (find-df-clause-data :aliases df-clauses))
       `(progn 
          (wb::redefinition-check ',name :define-function)
          (lisp:eval-when (:compile-toplevel :load-toplevel :execute)
            (setf (get ',name :define-function-parse) ',df-clauses))
          (lisp:eval-when (:load-toplevel :execute)
            (setf (get ',name :procedure-definition) ',*df-form*)
            (setf (get ',name :procedure-definition-time) (get-universal-time)))
          ,(generate-define-function-macro df-clauses name :expandable expandable) ; *****
          ,(generate-define-function-function df-clauses name :EXPANDABLE expandable :MV mv) ; *****
          ,@(generate-define-function-document-function-form df-clauses name)
          ,@(generate-define-function-goo df-clauses name)
          (record-df-source-file ',name)
          ',name
          )))

		  
(defun generate-define-macro-forms (df-clauses name &KEY expandable mv)
  `(progn 
     (wb::redefinition-check ',name :define-macro)
     (lisp:eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name :define-function-parse) ',df-clauses)
       (setf (get ',name :define-macro?) t))
     (lisp:eval-when (:load-toplevel :execute)
       (setf (get ',name :procedure-definition) ',*df-form*)
       (setf (get ',name :procedure-definition-time) (get-universal-time)))
     ,(generate-define-macro-external-macro df-clauses name :EXPANDABLE expandable) ; *****
     ,(generate-define-macro-internal-macro df-clauses name :EXPANDABLE expandable :MV mv) ; *****
     ,@(generate-define-function-document-function-form df-clauses name)
     ,@(generate-define-function-goo df-clauses name)
     (record-df-source-file ',name)
     ',name
     ))

(defun generate-defun-and-aliases (defun-form aliases)
  (if (null aliases)
      defun-form
    (let ((name (first (second defun-form))))
      `(progn 
         (defun ,name ,@(cddr defun-form))
         ,@(loop for alias in aliases collect 
                 `(defun ,alias (&rest args) (apply ',name args)))
         (lisp:eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(loop for alias in aliases collect
                   `(setf (get ',alias :alias-of) ',name))))
      )))

(defun df-function-name (name)
  (intern 
   (one-string "DF-FUNCTION-FOR-" (string name))
   (symbol-package name)
   ))

(defun df-macro-name (name)
  (intern 
   (one-string "DF-MACRO-FOR-" (string name))
   (symbol-package name)
   ))

(defun df-keyword-name (keyword name)
  (declare (ignore name))
  (intern (string keyword) *package* #+wrong (symbol-package name)))

(defun generate-define-function-macro (df-clauses name &KEY expandable) ; *****
  (let ((function-name (df-function-name name))
        (docstring (single-df-summary-string df-clauses)))
    `(lisp:defmacro ,name (&whole whole &body body)
       ,@(when docstring (list docstring))
       (declare (ignore body))
       (verify-df-call 
        ',function-name whole (get ',name :define-function-parse)
            :EXPANDABLE ,expandable)))) ; *****

(defun generate-define-macro-external-macro (df-clauses name &KEY expandable) ; *****
  (let ((macro-name (df-macro-name name))
        (docstring (single-df-summary-string df-clauses)))
    `(lisp:defmacro ,name (&whole whole &body body)
       ,@(when docstring (list docstring))
       (declare (ignore body))
       (verify-df-call 
        ',macro-name whole (get ',name :define-function-parse)
            :EXPANDABLE ,expandable)))) ; *****
#||

Any required variable that is not declared mapped must be 
coerced (if coercion is specified) and type-checked (if *safety* is T)
outside of the FLET body.  

Any required variable that is declared mapped but whose value
is in fact not a list should also be coerced (if coercion is specified)
and type-checked (if *safety* is T) outside of the FLET body.
(This is an optimization, because the coercion and type-checking 
could take place inside the FLET body, but then it would be repeated
every time the FLET was called.  Note that if the coercion actually
had side effects the difference would be noticeable, but we don't care;
coercions and type-checking should not have side effects!)

Required variables that are declared mapped and whose values
are indeed lists need to be coerced and type-checked inside
the FLET body because each value in the list needs to be 
coerced and type-checked.  

To do this then we need the list of possibly mapped required
variables, and whether in fact each variable is mapped.  We have this
information already via the compile time variable MAPPED-VARIABLES.
This is an assoc list where the second element is one of the declared
mapped variables, and the first element is a gensym variable which
will, at runtime, store whether or not the variable is indeed mapped
(that is, whether the value of the variable is a list).  (We actually
invert the assoc order in the subfunctions for convenience and sanity.)

||#

(DEFUN df-function-and-macro-argument-list
      (required-args token-args flag-args keyword-info
       keyword-argnames keyword-default-values name
       stack-vars stack-decls function? expandable?)
 
  (CONS
    (CONCATENATE 'LIST
        (IF expandable?
           `(&REST %%all-args%%)
           `(,@required-args 
             ,@token-args 
             ,@flag-args
             ,@(when keyword-info (list '&key))
             ,@(when keyword-info 
                  (loop for key in keyword-argnames 
                        for default-value in keyword-default-values
                        as lisp-key = (df-keyword-name key name)
                        collect 
                          (list lisp-key default-value)
                        ))))
        (IF function?
          `(&aux 
             (*df-name* ',name)
             ,@stack-vars)
          `(&aux (*df-name* ',name)))
     )
     (IF function?
        `(,@(when token-args `((declare (ignorable ,@token-args))))
         ,@stack-decls))
   ) 
)


(DEFUN DF-FUNCTION-AND-MACRO-LET 
          (args keyword-default-values required-args
           keyword-argnames flag-args token-args name
           any-mapping? actually-mapped?-list init-info 
           function? expandable? &REST body)
 
  (CONCATENATE 'LIST
   `(LET* (
     ,@(CONCATENATE 'LIST
         (IF expandable?
            `((,args (REVERSE %%all-args%%))
              (keyword-values (COPY-LIST ',keyword-default-values))
            ,@(WHEN required-args
                 (LOOP FOR arg IN required-args
                       COLLECT (LIST arg)))
            ,@(WHEN keyword-argnames
                 (LOOP FOR key IN keyword-argnames
                       AS lisp-key = (DF-KEYWORD-NAME key name)
                       COLLECT (LIST lisp-key)))
            ,@(WHEN flag-args
                 (LOOP FOR flag IN flag-args
                       COLLECT (LIST flag)))
            ,@(WHEN token-args
                 (LOOP FOR token IN token-args
                       COLLECT (LIST token)))
            ,@(LOOP FOR (g NIL) IN init-info
                    COLLECT (LIST g)))
                 ; For expandable, initializations done body, o/w in LET
           `(,@(CREATE-LET-FORMS-FROM-INIT-FORMS init-info)))
         (IF function?
             `(,@(loop for (g v) in actually-mapped?-list 
                     collect `(,g (listp ,v)))))
        )) 
   ,@(when token-args `((declare (ignorable ,@token-args))))
   ,@(when any-mapping? 
       `((declare (ignorable ,@(mapcar 'first actually-mapped?-list)))))
   ,@(FIRST body)))
)


(DEFUN DF-FUNCTION-AND-MACRO-EXPANDABLE-CODE
          (args required-args flag-args token-args
           keyword-argnames init-info) 
  `(
        (LOOP WITH value = NIL
              WITH keyword =  NIL
              WITH keyword-position = NIL
              WHILE (>= (LENGTH ,args) 
                        (+ (LENGTH ',required-args) (LENGTH ',flag-args) 
                           (LENGTH ',token-args) 2))
              WHILE ',keyword-argnames
              DO (SETF value (POP ,args))
                 (SETF keyword (POP ,args))
                 (COND
                    ((SETF keyword-position 
                             (POSITION keyword 
                                  ',keyword-argnames :TEST 'SYMBOL=))
                        (SETF (Nth keyword-position keyword-values) value))
                    (T (PUSH keyword ,args)
                       (PUSH value ,args)
                       (RETURN NIL))
                    )
               )
      ,@(LOOP FOR key IN keyword-argnames
              FOR i FROM 0
              COLLECT `(SETF ,key (NTH ,i keyword-values)))
              
      ,@(LOOP FOR flag IN (REVERSE flag-args)
              COLLECT `(SETF ,flag (POP ,args)))
      ,@(LOOP FOR token IN (REVERSE token-args)
              COLLECT `(SETF ,token (POP ,args)))
        (SETF ,args (REVERSE ,args))
        (IF (= (LENGTH ,args) 1)
            (SETF ,args (FIRST ,args)))
      ,@(LOOP FOR req-arg IN required-args
              FOR i FROM 1
              COLLECT
                (IF (= i (LENGTH required-args))
                   `(SETF ,req-arg ,args)
                   `(SETF ,req-arg (POP ,args))))
       ,@(LOOP for (g v) IN init-info
              COLLECT `(SETF ,g ,v))
   )
)


(DEFUN DF-function-mv-boilerplate-code
      (name required-args token-args flag-args keyword-argnames 
       init-vars convert-info type-info any-mapping?
       actually-mapped?-list 
       body-forms)
         
       `((without-code-walker 

           ;; As necessary, produce code to do conversions and do
           ;; type checking.

           ,@(generate-unmapped-convert-and-typecheck-clauses
              any-mapping? convert-info type-info required-args
              keyword-argnames actually-mapped?-list)

           ;; The actual body code, put into an FLET, with
           ;; a BLOCK wrapped around it, processed by the
           ;; code walker.  Also code to handle checking of
           ;; the return type if declared.
    ; ******** Differences relative to GENERATE-DEFINE-FUNCTION-FUNCTION below
             (let ((*bbl-level* (the fixnum (1+ *bbl-level*))))
                ,(let ((block-code 
                          (if (not (or *bbload-in-progress?*
                                       (bbl-mode?)))
                              `(block ,name ,@body-forms)
                            (with-symbols-in-bbl-environment 
                                `(,@required-args 
                                  ,@token-args
                                  ,@flag-args
                                  ,@keyword-argnames
                                  ,@init-vars)
                              (let ((*in-bbl-form-processor* t))
                                (bbl-code-walk `(block ,name ,@body-forms))
                                )))))
                      `,block-code)
               )))
)


(DEFUN DF-function-boilerplate-code 
      (name required-args token-args flag-args keyword-argnames 
       init-vars convert-info type-info any-mapping?
       mapped-variables actually-mapped?-list 
       mapcar? mapcarnn? maptree? crossmap?
       result-symbol result-symbols return-type values-return-type?
       body-forms)

 `((without-code-walker 

           ;; As necessary, produce code to do conversions and do
           ;; type checking.

           ,@(generate-unmapped-convert-and-typecheck-clauses
              any-mapping? convert-info type-info required-args
              keyword-argnames actually-mapped?-list)

           ;; The actual body code, put into an FLET, with
           ;; a BLOCK wrapped around it, processed by the
           ;; code walker.  Also code to handle checking of
           ;; the return type if declared.

           (flet 
               
               ((body-function (,@mapped-variables &aux ,@result-symbols) 
                  
                  ;; produce code to do conversions and type-checking
                  ;; for potentially mapped arguments

                  ,@(when any-mapping?
                      (generate-mapped-convert-and-typecheck-clauses
                       convert-info type-info required-args
                       actually-mapped?-list))
                  
                  ,(let ((block-code 
                          (if (not (or *bbload-in-progress?*
                                       (bbl-mode?)))
                              `(block ,name ,@body-forms)
                            (with-symbols-in-bbl-environment 
                                `(,@required-args 
                                  ,@token-args
                                  ,@flag-args
                                  ,@keyword-argnames
                                  ,@init-vars)
                              (let ((*in-bbl-form-processor* t))
                                (bbl-code-walk `(block ,name ,@body-forms))
                                )))))
                     (if (null values-return-type?)
                         `(setq ,result-symbol ,block-code)
                       `(multiple-value-setq ,result-symbols ,block-code)
                       ))

                  ,@(if (null values-return-type?)
                        (when return-type
                          `((when *safety* 
                              (verify-df-return-type 
                               ,result-symbol ',return-type ',name))))
                      `((when *safety* 
                          ,@(loop for rs in result-symbols 
                                  for rt in (cdr return-type)
                                  collect
                                  `(verify-df-return-type ,rs ',rt ',name)
                                  ))))
                  
                  ,(if (null values-return-type?)
                       result-symbol
                     `(values ,@result-symbols)
                     )))

             ;; Code to handle the various mapping options.

             (let ((*bbl-level* (the fixnum (1+ *bbl-level*))))

               ,(if (not any-mapping?) 
                    `(body-function)
                  (cond 
                   (mapcar? 
                    `(df-mapcar #'body-function ,@mapped-variables))
                   (mapcarnn? 
                    `(df-mapcarnn #'body-function ,@mapped-variables))
                   (maptree?
                    `(df-maptree #'body-function ,@mapped-variables))
                   (crossmap?
                    `(df-crossmap #'body-function ,@mapped-variables)
                    )))))))
)

(DEFUN generate-define-function-function (df-clauses name 
       &KEY expandable mv)
  ;; Grab all the information out of the parsed DEFINE-FUNCTION form

  (LET* ((function-name (df-function-name name))
         (required-args (all-required-args df-clauses))
         (token-args (all-token-args df-clauses))
         (flag-args (all-flag-args df-clauses))
         (keyword-data (find-df-clause-data :keyword df-clauses))
         (keyword-info (mapcar 'first keyword-data))
         (keyword-argnames (mapcar 'first keyword-info))
         (keyword-default-values (mapcar 'third keyword-info))
         (init-data (find-df-clause-data :init df-clauses))
         (init-info (mapcar 'first init-data))
         (init-vars (mapcar 'first init-info))
         (body-data (find-df-clause-data :body df-clauses))
         (body-forms (mapcar 'car body-data))
         (mapcar? (find-df-clause-data :mapcar df-clauses))
         (mapcarnn? (find-df-clause-data :mapcarnn df-clauses))
         (maptree? (find-df-clause-data :maptree df-clauses))
         (crossmap? (find-df-clause-data :crossmap df-clauses))
         (any-mapping? (or mapcar? mapcarnn? maptree? crossmap?))
         (mapped-variables 
          (when any-mapping? 
            (cond
             (mapcar? (mapcar 'first mapcar?))
             (mapcarnn? (mapcar 'first mapcarnn?))
             (maptree? (mapcar 'first maptree?))
             (crossmap? (mapcar 'first crossmap?))
             )))
         (actually-mapped?-list 
          (loop for v in mapped-variables
                collect (list (gensym (s+ (symbol-name v) "-MAPPED?-")) v)))
         (type-data (find-df-clause-data :type df-clauses))
         (type-info (mapcar 'first type-data))
         (return-type (caar (find-df-clause-data :return df-clauses)))
         (values-return-type? (values-return-type? return-type))
         (nvalues (if values-return-type? (length (cdr return-type)) 1))
         (convert-data (find-df-clause-data :convert df-clauses))
         (convert-info (mapcar 'first convert-data))
         (result-symbols 
          (loop for j from 1 to nvalues
                collect
                (gensym "RESULT-")))
         (result-symbol (first result-symbols))
         (args (GENSYM))
         )

    ;; Set up ability to do bbl stack tracing

    (multiple-value-bind (stack-vars stack-decls)
        (standard-definition-bbl-bindings)

    (CONCATENATE 'LIST
       `(lisp:DEFUN ,function-name) 
        (DF-FUNCTION-AND-MACRO-ARGUMENT-LIST
            required-args token-args flag-args keyword-info
            keyword-argnames keyword-default-values name
            stack-vars stack-decls T expandable)
        (LIST
         (CONCATENATE 'LIST
          (DF-FUNCTION-AND-MACRO-LET
            args keyword-default-values required-args
            keyword-argnames flag-args token-args name
            any-mapping? actually-mapped?-list init-info T expandable

            (CONCATENATE 'LIST
             (IF expandable
               (DF-FUNCTION-AND-MACRO-EXPANDABLE-CODE    
                 args required-args flag-args token-args keyword-argnames
                 init-info
                ))
             (IF mv
                 (DF-FUNCTION-mv-BOILERPLATE-CODE
                    name required-args token-args flag-args keyword-argnames 
                    init-vars convert-info type-info any-mapping?
                    actually-mapped?-list body-forms)
                 (DF-FUNCTION-BOILERPLATE-CODE name required-args token-args 
                    flag-args keyword-argnames 
                    init-vars convert-info type-info any-mapping?
                    mapped-variables actually-mapped?-list 
                    mapcar? mapcarnn? maptree? crossmap?
                    result-symbol result-symbols return-type values-return-type?
                    body-forms))
             )
          ))) 
        )
)))
	  
	  
;; Each &aux initialization must be evaluated in the context of 
;; all the passed in arguments and all the previously initialized
;; &aux variable values.  So each initialization form must be processed
;; in its own special context.  
(defun create-&aux-forms-from-init-forms 
       (init-forms required-args token-args flag-args keyword-argnames)
  (if (not (or *bbload-in-progress?* (bbl-mode?)))
      init-forms
    (let ((vars-so-far nil)
          (args (append required-args token-args flag-args keyword-argnames)))
      (with-symbols-in-bbl-environment 
          args
        (loop for (var init) in init-forms
              collect
              (with-symbols-in-bbl-environment 
                  (append vars-so-far (list var))
                (push var vars-so-far)
                (let ((*in-bbl-form-processor* t))
                  `(,var 
                    (without-code-walker
                      (let ((bbi::*bbl-level* 2))
                        ,(bbl-code-walk init)
                        ))))))))))

(DEFUN create-let-forms-from-init-forms (init-forms)
   (LOOP FOR (var init) IN init-forms
         COLLECT `(,var ,init)))


(defun generate-define-macro-internal-macro (df-clauses name &KEY expandable mv)
  ;; Grab all the information out of the parsed DEFINE-FUNCTION form
  (let* ((macro-name (df-macro-name name))
         (required-args (all-required-args df-clauses))
         (token-args (all-token-args df-clauses))
         (flag-args (all-flag-args df-clauses))
         (keyword-data (find-df-clause-data :keyword df-clauses))
         (keyword-info (mapcar 'first keyword-data))
         (keyword-argnames (mapcar 'first keyword-info))
         (keyword-default-values (mapcar 'third keyword-info))
         (body-data (find-df-clause-data :body df-clauses))
         (body-forms (mapcar 'car body-data))
         (args (GENSYM))
         (*result* (GENSYM))
         )
    (DECLARE (ignorable mv))
    (CONCATENATE 'LIST
       `(lisp:DEFMACRO ,macro-name) 
        (DF-FUNCTION-AND-MACRO-ARGUMENT-LIST
            required-args token-args flag-args keyword-info
            keyword-argnames keyword-default-values name
            NIL NIL NIL expandable)
        (LIST
         (CONCATENATE 'LIST
          (DF-FUNCTION-AND-MACRO-LET
            args keyword-default-values required-args
            keyword-argnames flag-args token-args name
            NIL NIL NIL NIL expandable
            (IF expandable
              (DF-FUNCTION-AND-MACRO-EXPANDABLE-CODE    
                args required-args flag-args token-args keyword-argnames 
                NIL))) ; Macros don't have initializations
          (LIST
           `(block ,name 
              (LET ((,*result* (PROGN ,@body-forms)))
                (LIST 'LET '((*BBL-LEVEL* (INCREMENT-BBL-LEVEL)))
                    ,*result*))))
          )) 
        )   
)) 

(defun define-function-verify-arg-form (var type required-args-list)
  `(verify-df-argument-type 
    ,var ',type ',var
    ,(if (member var required-args-list) :required :keyword)
    ))

(defmacro df-maybe-convert (argname to-type from-type conversion-name)
  `(when (typep ,argname ',from-type)
     (let ((f ,(if conversion-name 
                   `(find-named-defconversion ',to-type ',conversion-name)
                 `(find-default-defconversion ',to-type ',from-type))))
       (setq ,argname 
             (df-do-the-conversion
              f ,argname ',argname ',to-type ',from-type ',conversion-name))
       (return-from end-convert t)
       )))

(defun df-do-the-conversion 
       (f argvalue argname to-type from-type conversion-name)
  (handler-case (funcall f argvalue)
    (type-error 
     ()
     (dfee 
      (serr+
       problem "~S is trying to convert the value ~S"
       indent "of the argument named ~S,"
       indent "from type ~S to type ~S."
       indent "But no conversion method is defined (via DEFCONVERSION),"
       indent "and LISP:COERCE doesn't know how to do the conversion either."
       help~A )
      *df-name* argvalue argname 
      from-type to-type
      *df-name*))
    (error
     (c)
     (dfee
      (serr+
       problem "~S is trying to convert the argument named ~S,"
       indent "from type ~S to type ~S,"
       indent "using the ~S method. But this method itself generated an error!"
       indent "The error it signaled was:"
       indent "~A"
       help~A)
      *df-name* argname from-type to-type 
      (or conversion-name 'default) 
      c  *df-name* ))))
       

(defun generate-unmapped-convert-and-typecheck-clauses
       (any-mapping? convert-info type-info required-args keyword-argnames 
                     actually-mapped?-list)
  (declare (ignore any-mapping?))
  (let ((map (mapcar 'reverse actually-mapped?-list)))
    (append 
     (when convert-info 
       (generate-unmapped-convert-clauses
        convert-info required-args keyword-argnames map))
     (when type-info 
       (generate-unmapped-typecheck-clauses 
        type-info required-args map)))))

(defun generate-mapped-convert-and-typecheck-clauses
       (convert-info type-info required-args actually-mapped?-list)
  (let ((map (mapcar 'reverse actually-mapped?-list)))
    (append 
     (when convert-info 
       (generate-mapped-convert-clauses convert-info required-args map))
     (when type-info 
       (generate-mapped-typecheck-clauses type-info required-args map)))))

(defun generate-unmapped-convert-clauses
       (convert-info required-args keyword-args map)
  (flet ((maybe-convert-forms (argname conversions)
           `((block end-convert 
               ,@(create-df-maybe-convert-call argname conversions)))))
    (append
     (loop for argname in required-args
           as var-is-mapped? = (second (assoc argname map))
           as conversions = (all-conversions-for argname convert-info)
           append
           (when conversions 
             (if var-is-mapped? 
                 `((when (not ,var-is-mapped?)
                     ,@(maybe-convert-forms argname conversions)))
               (maybe-convert-forms argname conversions)
               )))
     (loop for argname in keyword-args 
           as conversions = (all-conversions-for argname convert-info)
           append 
           (when conversions (maybe-convert-forms argname conversions))
           ))))

(defun generate-mapped-convert-clauses (convert-info required-args map) 
  (flet ((maybe-convert-forms (argname conversions)
           `((block end-convert 
               ,@(create-df-maybe-convert-call argname conversions)))))
    (loop for argname in required-args
          as var-is-mapped? = (second (assoc argname map))
          as conversions = (all-conversions-for argname convert-info)
          append
          (when (and conversions var-is-mapped?) 
            `((when ,var-is-mapped?
                ,@(maybe-convert-forms argname conversions)
                ))))))

(defun generate-unmapped-typecheck-clauses (type-info required-args map)
  (vwhen (verify-arg-forms
          (remove-if 
           'null 
           (loop for (var type) in type-info
                 collect
                 (generate-unmapped-typecheck-clause 
                  var type required-args map)
                 )))
    `((when *safety* ,@verify-arg-forms))
    ))

(defun generate-mapped-typecheck-clauses (type-info required-args map)
  (vwhen (verify-arg-forms
          (remove-if 
           'null 
           (loop for (var type) in type-info
                 collect
                 (generate-mapped-typecheck-clause var type required-args map)
                 )))
    `((when *safety* ,@verify-arg-forms))
    ))
       
(defun generate-unmapped-typecheck-clause (var type required-args map)
  (vif (var-is-mapped? (second (assoc var map)))
       `(when (not ,var-is-mapped?) 
          ,(define-function-verify-arg-form var type required-args))
       (define-function-verify-arg-form var type required-args)
       ))

(defun generate-mapped-typecheck-clause (var type required-args map)
  (when (member var required-args)
    (vwhen (var-is-mapped? (second (assoc var map)))
      `(when ,var-is-mapped?
         ,(define-function-verify-arg-form var type required-args)))))


(defun create-df-maybe-convert-call (argname conversions) 
  (loop 
   for (nil to-type from-type conversion-name) in conversions
   collect 
   `(df-maybe-convert ,argname ,to-type ,from-type ,conversion-name)
   ))


(defun all-conversions-for (argname convert-info)
  (remove-if-not
   (lambda (x) (eq x argname)) convert-info :key 'first))

(defun verify-df-argument-type 
       (argument-value argument-type argument-name kind-of-argument)
  (unless (typep argument-value argument-type)
    (dfee
     (serr+
      problem "The value of the ~A argument named ~A to ~A is invalid."
      indent "The argument's value, ~A, is of type ~A."
      indent "This argument only allows values of type"
      indent "~A"
      advice "Change the value of this argument to be one of the above types."
      help~A)
     kind-of-argument argument-name *df-name* 
     (limited-form-string (formatn "~S" argument-value) 40 :single-line? t)
     (printed-type-of argument-value)
     (type-to-english-string argument-type)
     *df-name*
     )))

(defun verify-df-return-type (result return-type name)
  (unless (typep result return-type)
    (dfee
     (serr+
      problem "Invalid returned value for ~S:"
      indent "~S is trying to return ~S, of type:"
      indent "~A."
      indent "~S was defined to return value of type:"
      indent "~A."
      help~A)
     name name 
     (limited-form-string (formatn "~S" result) 40 :single-line? t)
     (printed-type-of result) name return-type name)))
         
(defun generate-define-function-document-function-form (df-clauses name)
  (let* ((aliases (find-df-clause-data :aliases df-clauses))
         (summary (single-df-summary-string df-clauses))
         (example-data (find-df-clause-data :example df-clauses))
         (example-strings (mapcar 'first example-data))
         (description (single-df-description-string df-clauses))
         (see-also-data (find-df-clause-data :see-also df-clauses))
         (see-also-symbols (mapcar 'first see-also-data))
         (author-data (find-df-clause-data :author df-clauses))
         (authors (mapcar 'first author-data))
         (return-data (find-df-clause-data :return df-clauses))
         (return-type 
          (if (any-mapping? df-clauses) 
              `(or list ,(if return-data (caar return-data) t))
            (caar return-data))))
    `((document-function 
          ,name 
	(:canonical t)
        (:flavor :define-function)
        ,@(when summary `((:summary ,summary)))
        ;; (:summary ,(if summary summary "No summary provided."))        
        (:syntax ,(generate-df-doc-syntax-form df-clauses name))
        (:returns nil :type ,(or return-type t)) 
        ,@(when aliases `((:synonyms ,@aliases)))
        ,@(when example-strings `((:examples ,@example-strings)))
        ,@(when description `((:text ,description)))
        (:parameters ,@(generate-df-doc-parameter-forms df-clauses name))
        ,@(when see-also-symbols `((:see-also ,@see-also-symbols)))
        ,@(when authors `((:author ,@authors)))
        ))))


(defun generate-df-doc-syntax-form (df-clauses name)
  (let* ((required-data (find-df-clause-data :required df-clauses))
         (required-and-token-args (mapcar 'first required-data))
         (flag-args (all-flag-args df-clauses))
         (keyword-data (find-df-clause-data :keyword df-clauses))
         (keyword-info (mapcar 'first keyword-data))
         (keyword-argnames (mapcar 'first keyword-info))
         )
    `(,name 
      ,@(mapcar 
         (lambda (x) (if (listp x) (cons :noise x) x))
         required-and-token-args)
      ,@(when flag-args
          (mapcar (lambda (f) `(:flag ,f)) flag-args))
      ,@(when keyword-argnames
          (mapcar (lambda (k) `(:keyword ,k)) keyword-argnames))
      )))

(defun any-mapping? (df-clauses)
  (or (find-df-clause-data :mapcar df-clauses)
      (find-df-clause-data :maptree df-clauses)
      (find-df-clause-data :crossmap df-clauses)))
           
              
(defun generate-df-doc-parameter-forms (df-clauses name)
  (declare (ignore name))
  (let* ((required-args (all-required-args df-clauses))
         (token-args (all-token-args df-clauses))
         (flag-args (all-flag-args df-clauses))
         (keyword-data (find-df-clause-data :keyword df-clauses))
         (keyword-info (mapcar 'first keyword-data))
         (keyword-argnames (mapcar 'first keyword-info))
         (keyword-default-values (mapcar 'third keyword-info))
         (mapcar? (find-df-clause-data :mapcar df-clauses))
         (maptree? (find-df-clause-data :maptree df-clauses))
         (crossmap? (find-df-clause-data :crossmap df-clauses))
         (any-mapping? (or mapcar? maptree? crossmap?))
         (mapped-variables 
          (when any-mapping? 
            (cond
             (mapcar? (mapcar 'first mapcar?))
             (maptree? (mapcar 'first maptree?))
             (crossmap? (mapcar 'first crossmap?))
             )))
         (type-data (find-df-clause-data :type df-clauses))
         (type-info (mapcar 'first type-data))
         (convert-data (find-df-clause-data :convert df-clauses))
         (convert-info (mapcar 'first convert-data)))
    (apply 
     'append
     (loop for req in required-args
           as argtype = (df-argtype req type-info)
           as convert-froms = (df-conversion-from-types req convert-info)
           as mapvar? = (member req mapped-variables)
           as maptype = (cond 
                         (mapcar? :mapcar)
                         (maptree? :maptree)
                         (crossmap? :crossmap))
           collect 
           `(,req 
             :parameter-type required :value-type ,argtype
             ,@(when convert-froms `(:can-convert-from ,convert-froms))
             ,@(when mapvar? `(:mapping-style ,maptype))
             ))
     (loop for tok in token-args collect
           `(,tok 
             :parameter-type token 
             :value-type boolean
             ))
     (loop for flag in flag-args collect
           `(,flag
             :parameter-type flag
             :value-type boolean
             ))
     (list
      (loop for key in keyword-argnames
            for default-value in keyword-default-values 
            as argtype = (df-argtype key type-info)
            as convert-froms = (df-conversion-from-types key convert-info)
            collect
            `(,key 
              :parameter-type keyword
              :value-type ,argtype 
              :default-value ,default-value 
              ,@(when convert-froms `(:can-convert-from ,convert-froms))
              )))
     )))

(defun df-argtype (argname type-info)
  (or (second (find argname type-info :key 'first)) t))

(defun df-conversion-from-types (argname convert-info)
  (mapcar 
   'third 
   (remove-if-not (lambda (x) (eq x argname)) convert-info :key 'first)))

(defun single-df-summary-string (df-clauses)
  (vwhen (summary-data (find-df-clause-data :summary df-clauses))
    (let ((summary-strings (mapcar 'first summary-data)))
      (string-join summary-strings)
      )))

(defun single-df-description-string (df-clauses)
  (vwhen (description-data (find-df-clause-data :description df-clauses))
    (let ((description-strings (mapcar 'first description-data)))
      (string-join description-strings)
      )))

(defun generate-define-function-goo (df-clauses name)
  (let ((aliases (find-df-clause-data :aliases df-clauses))
        (export-packages
         (mapcar 'car (find-df-clause-data :export-from df-clauses)))
        (public? (find-df-clause :public df-clauses)))
    (append 
     (loop for alias in aliases collect 
           `(lisp:defmacro ,alias (&body body) `(,',name ,@body)))
     (when public? 
       `((eval-when (:compile-toplevel :load-toplevel :execute)
           (lisp:import ',(cons name aliases) :bbl)
           (lisp:export ',(cons name aliases) :bbl)
           )))
     (loop for package in export-packages collect 
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (lisp:import ',(cons name aliases) ,(keywordize package))
              (lisp:export ',(cons name aliases) ,(keywordize package))
              ))
     (when aliases 
       `((lisp:eval-when (:compile-toplevel :load-toplevel :execute)
          ,@(loop for alias in aliases collect
                  `(setf (get ',alias :alias-of) ',name)))))
     )))
               
