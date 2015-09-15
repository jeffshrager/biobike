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

(defvar *in-bbl-form-processor* nil)
(defvar *bbload-in-progress?* nil)
(defvar *suppress-stack-printout-on-error* nil)
(declaim (fixnum *bbl-level*))
(defvar *bbl-level* 0)

(defun bbl-toplevel? () (= 1 *bbl-level*))

(defun bbl-loop-level-tracking-binding ()
  `((*bbl-level* (if (zerop *bbl-level*) 2 (the fixnum (1+ *bbl-level*))))))

(defun symbol-designates-frame? (symbol)
  (find #\. (symbol-name symbol)))

(defun frame-designator->frame (symbol)
  (let ((name (symbol-name symbol)))
    (if (char= #\. (char name 0)) 
        (frame-fnamed (subseq name 1) t)
      (frame-fnamed name t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fp-environment* nil)
(defvar *frame-bindings* nil)
(defvar *in-user-macro?* nil)
(defvar *trace-all-user-macros?* nil)

(defmacro in-user-macro (&body body)
  `(let ((*in-user-macro?* t)) ,@body))

(defun in-bbl-environment? (symbol)
  (find symbol *fp-environment* :test 'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-symbol-to-bbl-environment (symbol)
  (push symbol *fp-environment*))

(defmacro with-symbol-in-bbl-environment (symbol &body body)
  `(let ((*fp-environment* (cons ,symbol *fp-environment*)))
     ,@body))  

(defmacro with-symbols-in-bbl-environment (symbols &body body)
  `(let ((*fp-environment* (append ,symbols *fp-environment*)))
     ,@body))

(defun bbl-compile-repl-form (input-form repl)
  (with-standard-toplevel-bbl-bindings
    (let ((lisp-form 
           (bbl-form-to-lisp-form
            (wb::wrap-defun-with-locally-hack input-form))))
      (if (not (consp lisp-form))
          (values lisp-form nil)
        (values 
         input-form 
         (wb::compile-form-to-function-object lisp-form repl)
         )))))

(defun bbl-form-to-lisp-form (form)
  (bbl-form-processor form))

(defmacro with-bbl-form (&body body)
  #.(one-string-nl
     "Executes BODY in the BBL environment as an implicit PROGN which"
     "behaves like a toplevel form except that the standard stack trace"
     "printout when an error occurs is not done.")
  `(with-standard-toplevel-bbl-bindings 
     (let ((*readtable* *bbl-readtable*)
           (*bbl-level* 2)
           (*suppress-stack-printout-on-error* t))
       ,(bbl-form-processor `(progn ,@body))
       )))   

(defmacro wbf (&body body) `(with-bbl-form ,@body))
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (import '(wbf with-bbl-form) :bio))

(defun bbl-form-processor (form &key (only-dotted-symbols? nil))
  #.(one-string-nl
     "Takes a BBL form without []-notation and converts it"
     "into a standard lisp form that can be compiled and executed by the usual"
     "Weblistener form processing algorithms."
     ""
     "The processor finds all references to non-lexically bound symbols"
     "that actually represent frames, and optionally modifies FORM to"
     "keep a BBL execution stack (depending on *safety*).")
  (if only-dotted-symbols? 
      (convert-dotted-symbols form)
    (let ((*in-bbl-form-processor* t)
          (*fp-environment* nil)
          (*frame-bindings* nil)
          (*in-user-macro?* nil))
      (cond
       ((symbolp form) (symbol-form-processor form))
       ((atom form) form)
       (t (complex-form-processor form))
       ))))

(defun bbcl-form-processor (form)
  (let ((*in-bbl-form-processor* t)
        (*fp-environment* nil)
        (*frame-bindings* nil)
        (*in-user-macro?* nil))
    (cond
     ((symbolp form) (symbol-form-processor form))
     ((atom form) form)
     (t (bbcl-complex-form-processor form))
     )))

(defun symbol-form-processor (symbol)
  #.(one-string-nl
     "If SYMBOL is in the lexical environment, return it as is."
     "If otherwise the symbol represents a frame, return the frame."
     "If otherwise the symbol designates a frame (because it has '.' in it),"
     "then return the frame so designated (creating it if necessary)."
     "Otherwise return the symbol as is.")
  (cond
   ((or (null symbol) (eq symbol t)) symbol)
   ((constantp symbol) symbol)
   ((null (symbol-package symbol)) symbol)
   ((in-bbl-environment? symbol) symbol)
   (t 
    (vif (mapping (forward-funcall 'in-bbl-frame-mapping? symbol))
         mapping 
         (if (symbol-designates-frame? symbol)
             (frame-designator->frame symbol)
           symbol
           )))))

(defun verify-bbl-lvalue-symbols (symbols)
  (let ((frame-designators nil) (bad-symbols nil))
    (loop for s in symbols do
          (cond
           ((in-bbl-environment? s) nil)
           ((forward-funcall 'in-bbl-frame-mapping? s)
            (push s frame-designators))
           ((symbol-designates-frame? s) (push s bad-symbols))
           ))
    (when frame-designators
      (error
       (one-string-nl
        "You cannot assign a value to a symbol which designates a frame!"
        (one-string 
         "The symbol(s) " bioutils::*english-and-list* " denote frames,")
        "and so you cannot assign a value to them yourself.")
       frame-designators
       ))
    (when bad-symbols
      (error
       (one-string-nl        
        (one-string 
         "Your use of " bioutils::*english-and-list* " is illegal!")
        "The BBL language does not allow any symbol with a '.' in it"
        "to be given a value.  Symbols with '.' always name frames; they"
        "are not 'real' symbols.")
       bad-symbols
       ))))


(defun lambda-form? (form)
  (and (listp form) (eq (first form) 'lambda) (listp (second form))))

(defun complex-form-processor (form)
  (multiple-value-bind (toplevel-bindings toplevel-declares)
      (standard-toplevel-bbl-bindings)
    (let ((new-form (bbl-code-walk form)))
      `(let* (,@toplevel-bindings
             ,@*frame-bindings*
             )
         ,@toplevel-declares
         (handler-bind 
             ((error (lambda (c) (bbl-toplevel-error c))))
           ,new-form
           )))))

(defun bbcl-complex-form-processor (form) (bbl-code-walk form))

(defun bbl-toplevel-error (c) 
  (when (not *suppress-stack-printout-on-error*)   
    (format t "~%*** An error occurred...~%")
    (loop for index from (1- *user-stack-index*) downto 0
          for limit from 1 to 10 
          with indent = " " 
          with first? = t
          as form-string = (aref *user-stack* index)
          as function-string = (form-string-function-string form-string)
          as bbl-symbol = (find-symbol (string-upcase function-string) :bbl)
          as exported? = (and bbl-symbol 
                              (external-symbol-of-package? bbl-symbol :bbl))
          do 
          (when (and (or exported? (wb::weblistener-guru-p wb::*username*))
                     (not (eq bbl-symbol 'lisp:block)))
            (format t "~A~A ~A  :: ~A~%" 
                    indent 
                    (if first? "in the call to" "which was called by")
                    (or bbl-symbol function-string)
                    (limited-string form-string 30)
                    )
            (setq indent (s+ indent " "))
            (setq first? nil)
            ))
    (when (> *user-stack-index* 10)
      (formatt "~%The toplevel call was: ~A~%" 
               (limited-string (aref *user-stack* 0) 40)
               )))
  (error c)
  )

(defun form-string-function-string (fs)
  ;; get rid of initial left paren
  (let ((pos (position #\Space fs)))
    (when pos (subseq fs 1 pos))
    ))
    

(defun bbl-code-walk (form &optional (from-user-macro? nil))
  #.(one-string-nl
     "Recursively callable code walker for BBL forms."
     "Processes symbols according to BBL frame environment"
     "and calls appropriate subroutine for lambdas or other forms.")
  (macrolet ((not-in-user-macro (&body body) 
               `(let ((*in-user-macro?* from-user-macro?))
                  ,@body)))
    (cond 
     ((symbolp form) (not-in-user-macro (symbol-form-processor form)))
     ((atom form) form)
     (t 
      (let ((symbol (first form)))
        (cond
         ((lambda-form? symbol) 
          (not-in-user-macro (bbl-code-walk-lambda-call form)))
         ((symbolp symbol) 
          (cond 
           ((bbl-handle-specially? symbol) 
            (not-in-user-macro (bbl-code-walk-special form)))
           ((member symbol cl-user::*common-lisp-special-operators-and-macros*
                    :test 'symbol=)
            (not-in-user-macro (bbl-code-walk-common-lisp-form form)))
           ((special-operator-p symbol)
            (error "Ruh roh! Non-common lisp special operator ~A!!" symbol))
           ((macro-function symbol) 
            (bbl-code-walk-user-macro form))
           (t 
            (not-in-user-macro
              (stack-wrap 
               form
               `(,symbol ,@(mapcar 'bbl-code-walk (rest form)))
               )))))
         (t 
          (error 
           (one-string-nl
            "Invalid form ~S."
            "A form that is meant to be executed cannot have a first element"
            "which is a list.  It must be a function name."
            "You began this form with ~S."
            "(This could indicate a problem with the surrounding code; you may"
            "need to remove an encasing set of parentheses or perhaps use"
            "PROGN to execute two or more statements.)")
           (limited-form-string form 60)
           (limited-form-string symbol 50)
           ))))))))

(defun bbl-code-walk-lambda-call (form)
  (stack-wrap 
   form
   `(,(bbl-code-walk-lambda-form (first form))
     ,@(mapcar 'bbl-code-walk (cdr form))
     )))

(defvar *bbl-loop-symbol* nil)

(defun set-bbl-loop-symbol ()
  (unless *bbl-loop-symbol* 
    (unless (find-package :bbl) 
      (error "Internal error.  BBL package not defined"))
    (setq *bbl-loop-symbol* (find-symbol "LOOP" :bbl))))

(defun bbl-handle-specially? (symbol)
  (set-bbl-loop-symbol)
  (or (eq symbol *bbl-loop-symbol*)
      (member symbol 
              '(bbi::xloop
                document-function
                without-code-walker
                define-function 
                ))))

(defmacro without-code-walker (&body body)
  `(progn ,@body))   

(defun bbl-code-walk-special (form)
  (if (eq (first form) *bbl-loop-symbol*)
      (macroexpand form)
    (ecase (first form)
      (bbi::xloop
       (macroexpand form)
       )
      (document-function form)
      (without-code-walker `(progn ,@(cdr form)))
      ;; (define-function (macroexpand-1 form))
      (define-function form)
      )))

(defun descend-into-cdr (form)
  `(,(first form) ,@(mapcar 'bbl-code-walk (rest form))))

(defun descend-into-cddr (form)
  `(,(first form) ,(second form) ,@(mapcar 'bbl-code-walk (cddr form))))

(defun descend-into-form (form)
  (mapcar 'bbl-code-walk form))

(defun stack-wrap (original-form form)
  (if (and *in-user-macro?* (not *trace-all-user-macros?*))  
      form
    `(with-bbl-stack-trace ,original-form ,form)))
      
(defun bbl-code-walk-common-lisp-form (form &aux (symbol (first form)))

  (flet ((wrap (eform) (stack-wrap form eform)))

    (case (keywordize (s+ "=" (string symbol)))
      
      ;; special forms
      (:=block (wrap (descend-into-cddr form)))
      (:=flet (bbl-code-walk-flet-form form))
      (:=function 
       (if (lambda-form? (second form))
           `(,(first form) ,(bbl-code-walk-lambda-form (second form)))
         form))
      (:=if (wrap (descend-into-cdr form)))
      (:=let (bbl-code-walk-let-form form))
      (:=let* (bbl-code-walk-let*-form form))
      (:=handler-case (bbl-code-walk-handler-case-form form))
      (:=locally (bbl-code-walk-locally-form form))
      (:=progn (wrap (descend-into-cdr form)))
      (:=quote (bbl-code-walk-quote-form form)) 
      (:=return-from (wrap (descend-into-cddr form)))
      (:=setq 
       (wrap 
        `(,(first form) 
          ,@(loop for x on (rest form) by #'cddr
                  for var = (first x)
                  for value = (second x)
                  do (verify-bbl-lvalue-symbols (list var))
                  append `(,var ,(bbl-code-walk value))
                  ))))
      (:=tagbody (bbl-code-walk-tagbody-form form))
      (:=the (wrap (descend-into-cddr form)))
      (:=unwind-protect (wrap (descend-into-cdr form)))

      ;; macros
      (:=and (wrap (descend-into-cdr form)))
      (:=case (bbl-code-walk-case-form form))
      (:=cond (bbl-code-walk-cond-form form))
      (:=decf (bbl-code-walk-incf-decf-form form))
      (:=declaim form)
      (:=defconstant (bbl-code-walk-define-variable-form form))
      (:=defpackage form)
      (:=defparameter (bbl-code-walk-define-variable-form form))
      (:=defstruct form)
      (:=defun (bbl-code-walk-defun-form form))
      (:=defvar  (bbl-code-walk-define-variable-form form))
      (:=destructuring-bind (bbl-code-walk-bind-form form))
      (:=dolist (bbl-code-walk-dolist-dotimes-form form))
      (:=dotimes (bbl-code-walk-dolist-dotimes-form form))
      (:=ecase (bbl-code-walk-case-form form))
      (:=etypecase (bbl-code-walk-case-form form))
      (:=ignore-errors (wrap (descend-into-cdr form)))
      (:=in-package form)
      (:=incf (bbl-code-walk-incf-decf-form form))
      (:=lambda (bbl-code-walk-lambda-form form))
      (:=multiple-value-bind (bbl-code-walk-bind-form form))
      (:=multiple-value-list (wrap (descend-into-cdr form)))
      (:=multiple-value-setq 
       (verify-bbl-lvalue-symbols (second form))
       (wrap (descend-into-cddr form)))
      (:=or (wrap (descend-into-cdr form)))
      (:=pop (wrap `(,(first form) ,(descend-into-place (second form)))))
      (:=prog1 (wrap (descend-into-cdr form)))
      ((:=push :=pushnew)
       (wrap 
        `(,(first form) 
          ,(bbl-code-walk (second form))
          ,(descend-into-place (third form)))))
      (:=return (wrap (descend-into-cdr form)))
      (:=setf 
       (wrap 
        `(,(first form) 
          ,@(loop for x on (rest form) by #'cddr
                  for place = (first x)
                  for value = (second x)
                  append `(,(descend-into-place place)
                           ,(bbl-code-walk value))
                  ))))
      (:=time (wrap (descend-into-cdr form)))
      (:=trace form)
      (:=typecase (bbl-code-walk-case-form form))
      (:=unless (wrap (descend-into-cdr form)))
      (:=untrace form)
      (:=when (wrap (descend-into-cdr form)))
      (:=with-open-file (bbl-code-walk-with-open-file-form form))
      (otherwise form)
      )))

(defun convert-dotted-symbols (form)
  (cond
   ((symbolp form)
    (if (symbol-designates-frame? form)
        (frame-designator->frame form)
      form
      ))
   ((atom form) form)
   (t
    (multiple-value-bind (len list-type)
        (length-circular-or-dotted? form)
      (declare (ignore len))
      (case list-type
        (:proper (mapcar 'convert-dotted-symbols form))
        (:circular (error "Circular list structure!!"))
        (:dotted
         (let ((last-cell (last form)))
           (append
            (mapcar 'convert-dotted-symbols (butlast form))
            (cons
             (convert-dotted-symbols (car last-cell))
             (convert-dotted-symbols (cdr last-cell))
             )))))))))

(defun convert-quoted-symbols (form)
  (cond
   ((symbolp form)
    (if (symbol-designates-frame? form)
        (frame-designator->frame form)
      (vif (frame (utils::forward-funcall 'in-bbl-frame-mapping? form))
           frame
           form
           )))
   ((atom form) form)
   (t
    (multiple-value-bind (len list-type)
        (length-circular-or-dotted? form)
      (declare (ignore len))
      (case list-type
        (:proper (mapcar 'convert-quoted-symbols form))
        (:circular (error "Circular list structure!!"))
        (:dotted
         (let ((last-cell (last form)))
           (append
            (mapcar 'convert-quoted-symbols (butlast form))
            (cons
             (convert-quoted-symbols (car last-cell))
             (convert-quoted-symbols (cdr last-cell))
             )))))))))
  
(defun bbl-code-walk-quote-form (form)
  `(,(first form) ,(convert-quoted-symbols (second form))))
    

(defun bbl-canonicalize-let-binding (x)
  (cond 
   ((symbolp x) (list x nil))
   ((= 1 (length x)) (list (first x) nil))
   (t x)
   ))

(defun bbl-code-walk-let-form (form)
  (let ((bindings (second form))
        (body (cddr form)))
    (multiple-value-bind (doc decls body-forms errors)
        (parse-doc-decls-body body)
      (declare (ignore doc errors))
      (setq bindings (mapcar 'bbl-canonicalize-let-binding bindings))
      (stack-wrap 
       form 
       `(,(first form) 
         (,@(mapcar
             (lambda (b) `(,(first b) ,(bbl-code-walk (second b))))
             bindings))
         ,@(with-symbols-in-bbl-environment 
               (mapcar 'first bindings)
             (append decls (mapcar 'bbl-code-walk body-forms))
             ))))))

(defun bbl-code-walk-let*-form (form)
  (let ((bindings (second form))
        (body (cddr form)))
    (multiple-value-bind (doc decls body-forms errors)
        (parse-doc-decls-body body)
      (declare (ignore doc errors))
      (setq bindings (mapcar 'bbl-canonicalize-let-binding bindings))
      (stack-wrap 
       form 
       (with-symbols-in-bbl-environment
           nil
         `(,(first form)
           (,@(mapcar
               (lambda (b) 
                 (prog1
                     `(,(first b) ,(bbl-code-walk (second b)))
                   (add-symbol-to-bbl-environment (first b))
                   ))
               bindings))
           ,@(append decls (mapcar 'bbl-code-walk body-forms))
           ))))))

(defun bbl-code-walk-bind-form (form)
  (let* ((arglist (second form))
         (args (set-difference (flatten arglist) '(&optional &rest &key &aux)))
         (expression (third form))
         (body (cdddr form)))
    (multiple-value-bind (doc decls body-forms errors)
        (parse-doc-decls-body body)
      (declare (ignore doc errors))
      (stack-wrap 
       form
       `(,(first form) ,arglist 
            ,(bbl-code-walk expression)
          ,@(with-symbols-in-bbl-environment 
                args 
              (append decls (mapcar 'bbl-code-walk body-forms))
              ))))))
                                

(defun bbl-code-walk-handler-case-form (form)
  (let ((body-form (second form))
        (error-clauses (cddr form)))
    `(,(first form) 
      ,(stack-wrap 
        body-form
        (bbl-code-walk body-form))
      ,@(mapcar 
         (lambda (error-clause) 
           (destructuring-bind
               (condition-name condition-binding &rest condition-forms)
               error-clause 
             (with-symbols-in-bbl-environment (list condition-binding)
               `(,condition-name
                 ,condition-binding 
                 ,@(mapcar 'bbl-code-walk condition-forms)
                 ))))
         error-clauses 
         ))))

(defun bbl-code-walk-locally-form (form)
  (let ((body (cdr form)))
    (multiple-value-bind (doc decls body-forms errors)
        (parse-doc-decls-body body)
      (declare (ignore doc errors))
      (stack-wrap 
       form 
       `(,(first form) 
          ,@(append decls (mapcar 'bbl-code-walk body-forms))
          )))))

(defun bbl-code-walk-tagbody-form (form)
  `(,(first form)
    ,@(mapcar 
       (lambda (step) 
         (if (symbolp step) step (bbl-code-walk step)))
       (cdr form)
       )))

;; DOES NOT PROCESS LEXICAL FUNCTION DEFINITIONS!
(defun bbl-code-walk-flet-form (form)
  (destructuring-bind (flet function-definitions &rest body-forms) form
    `(,flet 
      ,function-definitions
      ,@(mapcar 'bbl-code-walk body-forms)
      )))
            

;;; Does not STACK-WRAP, since not executable in and of itself (sort of).

(defun bbl-code-walk-lambda-form (form)
  (let ((arglist (second form))
        (body (cddr form)))
    (multiple-value-bind (doc decls body-forms errors)    
        (parse-doc-decls-body body)
      (declare (ignore doc errors))
      `(,(first form) ,arglist
        ,@(with-symbols-in-bbl-environment
              arglist
            (append decls (mapcar 'bbl-code-walk body-forms))
            )))))

;; This is not completely implemented...we'd need to parse the entire
;; argument list but not doing that yet.

(defun bbl-code-walk-defun-form (form)
  (let ((name (second form))
        (arglist (third form))
        (body (cdddr form)))
    (multiple-value-bind (argnames initforms) 
        (walk-standard-lambda-list arglist nil)
      (declare (ignore initforms))
      (multiple-value-bind (doc decls body-forms errors)    
          (parse-doc-decls-body body)
        (declare (ignore errors))
        (multiple-value-bind (stack-vars stack-decls)
            (standard-definition-bbl-bindings)
          `(,(first form) 
            ,name ,arglist ,@(and doc (list doc)) ,@decls
            (let ,stack-vars 
              ,@stack-decls
              ,@(with-symbols-in-bbl-environment
                    argnames 
                  (mapcar 'bbl-code-walk body-forms)
                  ))))))))

(defun bbl-code-walk-case-form (form)
  (stack-wrap 
   form
   `(,(first form) ,(bbl-code-walk (second form))
     ,@(mapcar 
        (lambda (case-clause) 
          `(,(first case-clause) 
            ,@(mapcar 'bbl-code-walk (rest case-clause))))
        (cddr form)
        ))))

(defun bbl-code-walk-cond-form (form)
  (stack-wrap 
   form
   `(,(first form) ,@(mapcar 'descend-into-form (cdr form)))))


(defun bbl-code-walk-incf-decf-form (form)
  (stack-wrap 
   form
   `(,(first form) 
     ,(descend-into-place (second form))
     ,@(when (third form)
         (list (bbl-code-walk (third form)))))))



(defun bbl-code-walk-define-variable-form (form)
  (stack-wrap
   form
   `(,(first form)
     ,(second form) 
     ,@(when (cddr form) (list (bbl-code-walk (third form))))
     ,@(when (cdddr form) (list (fourth form))))))

(defun bbl-code-walk-dolist-dotimes-form (form)
  (let* ((name (first form))
         (iter (second form))
         (var (first iter))
         (body (cddr form)))
    (verify-bbl-lvalue-symbols (list var))
    (stack-wrap 
     form
     `(,name 
       (,var
        ,(bbl-code-walk (second iter))
        ,@(when (third iter)
            (with-symbols-in-bbl-environment 
                (list var)
              (list (bbl-code-walk (third iter))))))
       ,@(multiple-value-bind (doc decls body-forms errors)
             (parse-doc-decls-body body)
           (declare (ignore doc errors))
           (with-symbols-in-bbl-environment 
               (list var)
             (append decls (mapcar 'bbl-code-walk body-forms))
             ))))))

(defun bbl-code-walk-with-open-file-form (form)
  (let* ((spec (second form))
         (var (first spec))
         (stream (second spec))
         (other (cddr spec))
         (body (cddr form)))
    (stack-wrap
     form
     `(,(first form) 
          (,var
           ,(bbl-code-walk stream)
           ,@other
           )
        ,@(multiple-value-bind (doc decls body-forms errors)
              (parse-doc-decls-body body)
            (declare (ignore doc errors))
            (with-symbols-in-bbl-environment 
                (list var)
              (append decls (mapcar 'bbl-code-walk body-forms))
              ))))))

(defun descend-into-place (form)
  (flet ((descend-setf () 
           `(,(first form) 
             ,(descend-into-place (second form))
             ,@(mapcar 'bbl-code-walk (cddr form)))))
    (cond
     ((isframe? form) 
      (error "You cannot assign a value to ~S!  It is a frame." form))
     ((constantp form) 
      (error "You cannot assign a value to ~S!  It is a constant." form))
     ((symbolp form) 
      (verify-bbl-lvalue-symbols (list form)) form)
     ((not (listp form))
      (error "Do not know how to assign a value to ~S" form))
     ((member (first form) 
              '(utils:gref
                lisp:aref 
                lisp:row-major-aref 
                lisp:svref
                lisp:char
                lisp:schar
                lisp:get
                lisp:bit))
      (descend-setf))
     ;; make sure we don't recurse on the place if it is a frame.  
     ;; frames are only allowed as places within REF.  
     ((eq (first form) 'utils::ref)
      (flet ((descend-setf-args (place) 
               `(,(first form)
                 ,place ,@(mapcar 'bbl-code-walk (cddr form)))))
        (cond
         ((isframe? (second form)) (descend-setf-args (second form)))
         ((symbolp (second form))
          (let ((frame-or-symbol (symbol-form-processor (second form))))
            (if (isframe? frame-or-symbol)
                (descend-setf-args frame-or-symbol) 
              (descend-setf)
              )))
         (t (descend-setf))
         )))
     ((member (first form)
              '(lisp:gethash 
                lisp:nth
                lisp:nthcdr))
      `(,(first form)
        ,(bbl-code-walk (second form))
        ,(descend-into-place (third form))
        ))
     ((member (first form)
              '(lisp:car lisp:cdr lisp:first lisp:rest
                         lisp:cadr lisp:caar lisp:cddr lisp:cdar
                         lisp:second lisp:third lisp:fourth lisp:fifth
                         lisp:sixth lisp:seventh lisp:eighth lisp:ninth
                         ))
      `(,(first form) ,(descend-into-place (second form))))
     ((eq (first form) 'frames:slotv)
      (if (isframe? (second form))
          `(,(first form) ,(second form) ,@(mapcar 'bbl-code-walk (cddr form)))
        (descend-setf)
        ))
     ((eq (first form) 'lisp:subseq)
      (cond 
       ((= (length form) 3)
        `(,(first form) 
          ,(descend-into-place (second form))
          ,(bbl-code-walk (third form))))
       ((= (length form) 4)
        `(,(first form) 
          ,(descend-into-place (second form))
          ,(bbl-code-walk (third form))
          ,(bbl-code-walk (fourth form))))
       (t (error "Invalid subseq form: ~A" form))
       ))
     ((wlisp::specially-hacked-slotv-lambda? form)
      (descend-into-place 
       (wlisp::transform-slotv-lambda-place-into-slotv-place form)))
     (t 
      (error "Do not know how to process a place like ~S" form))
     )))

   
(defun bbl-process-loop-forms (forms vars)
  (with-symbols-in-bbl-environment 
      vars
    (loop for form in forms 
          collect (bbl-code-walk form)
          )))

(defun bbl-process-loop-bindings (bindings)
  (flet ((vl (s) (verify-bbl-lvalue-symbols (list s))))
    (loop for b in bindings
          collect
          (cond
           ((symbolp b) (vl b) b)
           ((null (cdr b)) (vl (first b)) b)
           ((null (second b)) (vl (first b)) b)
           (t 
            (vl (first b))
            (let ((init-form (second b)))
              (list 
               (first b)
               (let ((*safety* nil)) (bbl-code-walk init-form))
               )))))))


(defun bbl-code-walk-user-macro (form)
  (let ((new-form (macroexpand-1 form)))
    (stack-wrap 
     form
     (in-user-macro (bbl-code-walk new-form t)) 
     ))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hack-code-for-bbl-toplevel (code)
  (flet ((hack () 
           (cons 
            (car code)
            (mapcar 
             (lambda (arg) 
               (cond
                ((not (consp arg)) arg)
                (t `(let ((bbi::*bbl-level* 2)) ,arg))
                ))
             (cdr code)
             ))))
    (cond
     ((not (consp code)) code)
     (t 
      (let ((f (first code)))
        (cond
         ((not (symbolp f)) code)
         ((special-operator-p f) code)
         ((get f :define-macro?) code)
         ((help::define-function-p f) (hack))
         ((get f :alias-of) 
          (hack-code-for-bbl-toplevel `(,(get f :alias-of) ,@(cdr code))))
         ((macro-function f) code)
         (t (hack))
         ))))))

