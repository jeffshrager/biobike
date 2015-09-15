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

(defvar *df-name* nil 
  #.(one-string-nl
     "The name of the DEFINE-FUNCTION procedure currently being parsed."
     "or currently being executed."))

(defvar *df-type* nil
  "Currently either :DEFINE-FUNCTION or :DEFINE-MACRO.")

(defvar *df-form* nil 
  "The entire DEFINE-FUNCTION or DEFINE-MACRO form currently being parsed.")

(defvar *current-clause-keyword* nil
  #.(one-string-nl
     "The clause keyword the user specified while doing clause separation"
     "and parsing of a clause."))

(defvar *multiple-keyword-specs* nil
  "The currently-being-processed list of keyword specifications")

(defparameter *passes-to-do* 
  '(:separate :combine :parse :verify :codegen))

#|

The first pass of the DEFINE-FUNCTION parsing algorithm splits
the forms up into clauses.  A list of these parsed clauses is returned,
each clause element is of the form (clause-keyword clause-data clause-form).
The clause-data is of the form (datum1 datum2 ... datum-n) where each
datum is of a format specific to the clause-keyword it belongs to.   

The second pass merges equivalent clauses (most DEFINE-FUNCTION keywords
are allowed to be listed multiple times).  When we merge equivalent
clauses, in order to preserve the original clause each datum came from
we need to attach the clause-form to each datum.  Therefore once all 
equivalent clauses are merged into a single clause the merged clause
will be of the form 
(clause-keyword 
 ((d1 d1-clause-form) (d2 d2-clause-form) ... (dn dn-clause-form)))

The third pass verifies that each datum is syntactically valid.

The fourth pass verifies that all the data in all the clauses is
semantically valid (eg, it is syntactically correct to have a clause 
type x = fixnum, but if the symbol x is not one of the arguments to
the DEFINE-FUNCTION (or a declared local variable) then that clause
is semantically invalid).  

The fifth pass generates the code.  

|#

(defvar *separated-clauses* nil)
(defvar *combined-clauses* nil)
(defvar *parsed-clauses* nil)
(defvar *verified-clauses* nil)

(defun separated-clause-keyword (c) (first c))
(defun separated-clause-data (c) (second c))
(defun separated-clause-form (c) (third c))

(defun merged-clause-keyword (mc) (first mc))
(defun merged-clause-data (mc) (second mc))
(defun merged-data-datum (d) (first d))
(defun merged-data-form (d) (second d))

(DEFMACRO Test-for-special-arg (define-macro-parse special-flag-symbol)
  ; FLAG %%expandable%% in DEFINE-FUNCTION or DEFINE-MACRO definition
  ; causes last required argument to be expandable, taking any number of values
  ; FLAG %%multivalue%% causes the function to permit multiple values to be returned.
  ; Neither flag will appear in the list of flags of the function/macro
   `(LET* ((flag-pos (POSITION :flag ,define-macro-parse :KEY 'FIRST))
           (special-flag-pos 
            (IF flag-pos
                (POSITION ,special-flag-symbol 
                   (SECOND (Nth flag-pos ,define-macro-parse))
                   :TEST 'SYMBOL= :KEY 'FIRST)))
          )
      (WHEN special-flag-pos
        (SETF (Nth flag-pos ,define-macro-parse)
            (CONS :FLAG
                 (LIST (REMOVE-NTH-ELEMENT special-flag-pos 
                         (SECOND (Nth flag-pos ,define-macro-parse))))))
        T)
))

(define-condition df-parse-error (error) 
  ((explanation :initarg :explanation :reader explanation))
  (:report
   (lambda (condition stream)
     (format stream "~A" (explanation condition))
     )))

(defmacro define-df-syntactic-tokens (df-function-name &rest tokens)
  `(without-code-walker
     (setf (get ',df-function-name :syntactic-tokens) '(,@tokens))))

(defmacro define-function (&whole form name &body body)
  #.(one-string-nl
     "See the specification for DEFINE-FUNCTION for an in-depth description."
     "(Currently in .../Doc/define-function.txt)"
     "The point of DEFINE-FUNCTION is to provide a more english-like"
     "way to both define functions and to call them."
     "Features include: function-name aliases, type checking of arguments"
     "and return values, automatic coercion of input arguments, implicit"
     "mapping over certain specified required arguments, flag variables,"
     "syntactic sugar (token variables), and simpler keyword argument syntax.")
  (let* ((*df-name* name)
         (*df-type* :define-function)
         (*df-form* form)
         (ignore (parse-define-function-name name)) 
         (define-function-parse 
          (with-multiple-continuable-errors 
              (3 3 continue-df-parse 
                 (formatn "while parsing (DEFINE-FUNCTION ~S ...)" name) 
                 :errors-to-catch (df-parse-error))
            (parse-define-function form name body)))
         (expandable (TEST-FOR-SPECIAL-ARG define-function-parse '%%expandable%%)) ; *****
         (mv (TEST-FOR-SPECIAL-ARG define-function-parse '%%multivalue%%)) ; *****
             ; deletes expandable tag as a side-effect
         )
    (declare (ignore ignore))
    (if (member :codegen *passes-to-do*) 
        (forward-funcall 
         'generate-define-function-forms define-function-parse *df-name*
            :EXPANDABLE expandable :MV mv) ; *****
      `',define-function-parse
      )))
  
	  
(DEFMACRO define-macro (&whole form name &body body)
  #.(one-string-nl
     "An internal macro for use by Biobike language developers.  Analogous"
     "to DEFINE-FUNCTION but much simpler: it only deals with argument lists,"
     "not with argument types, coercion, return types, mapping or exporting."
     "(Providing documentation clauses is allowed.)"
     ""
     "DEFINE-MACRO is also much more restricted than DEFMACRO: it can only"
     "take a linear set of arguments, not a destructuring lambda list."
     "The point of DEFINE-MACRO is to be able to parse macro arguments in"
     "the same style as for DEFINE-FUNCTION.")
  (let* ((*df-name* name)
         (*df-type* :define-macro)
         (*df-form* form)
         (Define-macro-parse 
          (with-multiple-continuable-errors 
              (3 3 continue-df-parse 
                 (formatn "while parsing (DEFINE-MACRO ~S ...)" name) 
                 :errors-to-catch (df-parse-error))
            (parse-define-function form name body)))
         (expandable (TEST-FOR-SPECIAL-ARG define-macro-parse '%%expandable%%)) ; *****
         (mv (TEST-FOR-SPECIAL-ARG define-macro-parse '%%multivalue%%)) ; *****
             ; deletes expandable tag as a side-effect
         )
  
    (if (member :codegen *passes-to-do*) 
        (forward-funcall 
         'generate-define-macro-forms define-macro-parse *df-name* 
             :EXPANDABLE expandable :MV mv)  ; *****
      `',define-macro-parse
      )))

;;; This should do indenting a la XLOOP

(defun df-parse-error (format-string &rest format-args)
  (error
   (let ((header (formatn "In (~A ~S ...)~~%" *df-type* *df-name*)))
     (make-condition
      'df-parse-error
      :explanation 
      (apply 'format nil (s+ header format-string) format-args)
      ))))

(defun parse-define-function-name (name)
  (unless (or (symbolp name) (listp name))
    (error 
     (one-string-nl
      "In a (~A ...) form,"
      "The name provided, ~S, must be a symbol (the name),"
      "or a list of symbols (the first being the name, the rest aliases)."
      "But you provided something which is neither (of type ~S).")
     *df-type* name (printed-type-of name)
     ))
  (unless (or (symbolp name) (every 'symbolp name))
    (error 
     (one-string-nl
      "In a (~A ...) form,"
      "within the list of names and aliases: ~S,"
      "one or more of these is not a symbol: ~S."
      "(The name and all its aliases must be symbols).")
     *df-type* name (remove-if 'symbolp name)))
  (if (symbolp name) 
      (wb::redefinition-check name :define-function)
    (loop for n in name do (wb::redefinition-check n :define-function))
    ))

(defun parse-define-function (form name body)
  (setq *df-name* (or (and (symbolp name) name) (first name)))
  (unless body 
    (df-parse-error "The function definition has no clauses or body!"))
  (let ((args (first body))
        (clauses (rest body)))
    (cond
     ((listp args) 
      (unless clauses 
        (df-parse-error
         (one-string-nl
          "The function definition has no body!"
          "(Since the form after the name, ~S, is a list,"
          "it is interpreted as an argument list for a DEFUN-syntax"
          "DEFINE-FUCTION definition.  The form or forms after this"
          "argument list should be the body of the function, but no"
          "such forms exist...)")
         args))
      (parse-defun-like-define-function form name args clauses))
     ((symbolp args) 
      (parse-standard-define-function form name body))
     (t 
      (df-parse-error
       (one-string-nl
        "The third form, ~S,"
        "is neither a symbol (designating the start of a clause)"
        "nor an argument list (designating a DEFUN-syntax definition)."
        "It must be one or the other, not something of type ~S")
       args (type-of args)
       )))))

(defun parse-defun-like-define-function (form name args body)
  (declare (ignore form))
  (multiple-value-bind (real-name aliases)
      (verify-name-and-aliases name)
    (multiple-value-bind (doc decls body-forms bad-decls)
        (parse-doc-decls-body body)
      (declare (ignore bad-decls))
      (list 
       (list :name real-name)
       (list :aliases aliases)
       (list :form :defun)
       (list :docstring doc)
       (list :declarations decls)
       (list :body body-forms)
       (list :defun-like t)
       (list :defun
             `(defun ,name ,args ,@(and doc (list doc)) ,@decls ,@body-forms))
       ))))
  
(defun parse-standard-define-function (form name body)
  (declare (ignore form))
  (multiple-value-bind (real-name aliases)
      (verify-name-and-aliases name)
    (let ((additional-clauses 
           (append 
            (list (list :name real-name) (list :aliases aliases)))))
      (setq *separated-clauses* 
            (if (member :separate *passes-to-do*) 
                (separate-df-clauses body)
              nil))
      (setq *combined-clauses* 
            (if (member :combine *passes-to-do*)
                (combine-df-clauses *separated-clauses*)
              *separated-clauses*))
      (setq *parsed-clauses*
            (if (member :parse *passes-to-do*)
                (forward-funcall 'parse-df-clauses *combined-clauses*)
              *combined-clauses*))
      (setq *verified-clauses* 
            (if (member :verify *passes-to-do*)
                (forward-funcall 'verify-df-clauses *parsed-clauses*)
              *parsed-clauses*))
      (append additional-clauses *verified-clauses*))))


(defun verify-name-and-aliases (name)  
  (restart-case 
      (let ((real-name nil) (aliases nil))
        (cond 
         ((symbolp name) (setq real-name name))
         ((listp name) 
          (vwhen (duplicates (check-for-duplicates name)) 
            (df-parse-error 
             (one-string-nl
              "The list of aliases, ~S,"
              "contains these identical symbols: ~S."
              "(The name and all of its aliases must be unique.)")
             name duplicates 
             ))
          (setq real-name (first name))
          (setq aliases (rest name))
          (loop 
           for alias in aliases 
           do 
           (restart-case 
               (when (fboundp alias)
                 (cond
                  ((null (get alias :alias-of)) 
                   (df-parse-error  
                    (one-string-nl
                     "You are defining an alias ~S."
                     "But ~S names an already existing function, so this"
                     "previously existing function definition would be"
                     "overwritten!  If you really want to do this first"
                     "explicitly execute (fmakunbound '~S) which will"
                     "destroy the existing function definition.")
                    alias alias alias
                    ))
                  ((not (eq real-name (get alias :alias-of)))
                   (warn 
                    (one-string-nl
                     "In (~A ~S ...)"
                     "You are defining an alias ~S."
                     "But ~S is previously defined as an alias for ~S."
                     "This previous alias definition will be superseded.")
                    *df-type* *df-name* alias alias (get alias :alias-of)
                    ))
                  (t nil)
                  ))
             (continue-df-parse () nil)
             ))))
        (values real-name aliases))
    (continue-df-parse () (values nil nil))
    ))

