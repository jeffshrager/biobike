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

(defvar *current-cclauses* nil)

(defun parse-df-clauses (cclauses)
  (let ((*current-cclauses* cclauses))
    (let* ((parsed-clauses 
            (loop for clause in cclauses 
                  as clause-name = (first clause)
                  append
                  (restart-case 
                      (parse-df-clause clause-name clause)
                    (continue-df-parse () nil)))))
      parsed-clauses)))

(defun verify-df-args-are-symbols (data argtype)
  (let* ((symbols (mapcar 'merged-data-datum data))
         (clause-forms (mapcar 'merged-data-form data))
         (first-clause (first clause-forms))
         (all-clauses-same? 
          (every (lambda (form) (eq form first-clause)) clause-forms)))
    (unless (every 'symbolp symbols)
      (df-parse-error
       (one-string-nl
        (if all-clauses-same?
            "In the clause ~S ~A"
          "In multiple clauses defining the ~A variables ~S,")
        "one or more of the ~A variables you provided,"
        "~S,"
        "is not a symbol.  All ~A variables must be symbols.")
       (if all-clauses-same? first-clause argtype)
       (if all-clauses-same? "" symbols)
       argtype (remove-if 'symbolp symbols) argtype)
      )))

(defun verify-df-args-are-not-constants (data argtype)
  (let* ((symbols (mapcar 'merged-data-datum data))
         (clause-forms (mapcar 'merged-data-form data))
         (first-clause (first clause-forms))
         (all-clauses-same? 
          (every (lambda (form) (eq form first-clause)) clause-forms)))
    (when (some 'constantp symbols)
      (df-parse-error
       (one-string-nl
        (if all-clauses-same?
            "In the clause ~S ~A"
          "In multiple clauses defining the ~A variables ~S,")
        "one or more of the ~A variables you provided,"
        "~S,"
        "is already defined as a constant variable."
        "You are not allowed to redefine symbols which"
        "are already defined as constants.")
       (if all-clauses-same? first-clause argtype)
       (if all-clauses-same? "" symbols)
       argtype (remove-if-not 'constantp symbols)))))

(defun verify-df-args-no-equal-signs (data argtype)
  (let* ((symbols (mapcar 'merged-data-datum data))
         (clause-forms (mapcar 'merged-data-form data))
         (first-clause (first clause-forms))
         (all-clauses-same? 
          (every (lambda (form) (eq form first-clause)) clause-forms)))
    (when (some 'contains-equal-sign? symbols)
      (df-parse-error
       (one-string-nl
        (if all-clauses-same?
            "In the clause ~S ~A"
          "In multiple clauses defining the ~A variables ~S,")
        "one or more of the ~A variables you provided,"
        "~S,"
        "contains an '=' as part of its name."  
        "In biobike, you are not allowed to use '='"
        "as part of the name of any required, token, keyword"
        "or flag variable.")
       (if all-clauses-same? first-clause argtype)
       (if all-clauses-same? "" symbols)
       argtype (remove-if-not 'contains-equal-sign? symbols)))))

(defun verify-df-args-unique (data argtype)
  (let* ((symbols (mapcar 'merged-data-datum data))
         (clause-forms (mapcar 'merged-data-form data))
         (first-clause (first clause-forms))
         (all-clauses-same? 
          (every (lambda (form) (eq form first-clause)) clause-forms)))
    (vwhen (duplicates (check-for-duplicates symbols))
      (df-parse-error
       (one-string-nl
        (if all-clauses-same?
            "In the clause ~S ~A"
          "In multiple clauses defining the ~A variables ~S,")
        "one or more of the ~A variable names is"
        "not unique!  All required, token, keyword, and"
        "flag variables must have unique names."
        "The non-unique ~A variables are:"
        "~S.")
       (if all-clauses-same? first-clause argtype)
       (if all-clauses-same? "" symbols)
       argtype argtype duplicates))))

(defun verify-df-required-args (data)
  (verify-df-args-are-symbols data "required")
  (verify-df-args-are-not-constants data "required")
  (verify-df-args-no-equal-signs data "required")
  (verify-df-args-unique data "required")
  )

(defun verify-df-token-args (data)
  (loop for (token-args clause) in data do 
        (let ((token-data (mapcar (lambda (x) (list x clause)) token-args)))
          (verify-df-args-are-symbols token-data "token")
          (verify-df-args-are-not-constants token-data "token")
          ;; token arguments are allowed to contain '=' since
          ;; they are never initialized or typed or coerced 
          ;; (verify-df-args-no-equal-signs token-data "token")
          (verify-df-args-unique token-data "token")
          )))

(defgeneric parse-df-clause (name clause)
  (:documentation 
   #.(one-string-nl
      "Verifies that each clause is syntactically correct insofar as it"
      "has not yet been checked by the clause separation phase."
      "No semantic checking (such as whether a variable in a type clause"
      "is actually one of the function's arguments) is done here."
      "Some clauses are broken up into more than one clause.  Therefore"
      "this function returns a list of clauses, which are then appended"
      "together to reconstruct the entire (possibly augmented) clause set.")))

(defmethod parse-df-clause ((name t) clause)
  (list clause))
  

(defmethod parse-df-clause ((name (eql :required)) clause)
  (let ((required-data (merged-clause-data clause)))
    (multiple-value-bind (required-data token-data)
        (separate-into-lists 
         required-data
         (lambda (data) (not (consp (merged-data-datum data)))))
      (verify-df-required-args required-data)
      (verify-df-token-args token-data)
      (list clause 
            `(:required-args ,required-data)
            `(:token-args ,token-data)
            ))))

(defmethod parse-df-clause ((name (eql :flag)) clause)
  (parse-variable-list-df-clause name clause))

(defmethod parse-df-clause ((name (eql :mapcar)) clause)
  (parse-variable-list-df-clause name clause))

(defmethod parse-df-clause ((name (eql :mapcarnn)) clause)
  (parse-variable-list-df-clause name clause))

(defmethod parse-df-clause ((name (eql :maptree)) clause)
  (parse-variable-list-df-clause name clause))

(defmethod parse-df-clause ((name (eql :crossmap)) clause)
  (parse-variable-list-df-clause name clause))

(defmethod parse-df-clause ((name (eql :export-from)) clause)
  (let* ((data (merged-clause-data clause))
         (symbols (mapcar 'merged-data-datum data))
         (clause-forms (mapcar 'merged-data-form data))
         (first-clause (first clause-forms))
         (all-clauses-same? 
          (every (lambda (form) (eq form first-clause)) clause-forms)))
    (vwhen (duplicates (check-for-duplicates symbols))
      (df-parse-error
       (one-string-nl
        (if all-clauses-same?
            "In the EXPORT-FROM clause ~S,"
          "In multiple EXPORT-FROM clauses, ~A")
        "one or more of the package names is not unique!"
        "The non-unique package names are:"
        "~S.")
       (if all-clauses-same? first-clause "")
       duplicates))
    (list clause)))
  
(defun parse-variable-list-df-clause (name clause)
  (let ((data (merged-clause-data clause))
        (name (string name)))
    (verify-df-args-are-symbols data name)
    (verify-df-args-are-not-constants data name)
    (verify-df-args-no-equal-signs data name)
    (verify-df-args-unique data name)
    (list clause)
    ))

(defmethod parse-df-clause ((name (eql :keyword)) clause)
  (let* ((data (merged-clause-data clause))
         (keywords-and-aliases nil)
         (keyword-clauses nil))
    (loop for (keyword-datum keyword-clause) in data 
          as keyword-name = (first keyword-datum)
          as keyword-aliases = (second keyword-datum)
          as name-and-aliases = (cons keyword-name keyword-aliases)
          do
          (pushnew keyword-clause keyword-clauses)
          (setq keywords-and-aliases 
                (append name-and-aliases keywords-and-aliases))
          (vwhen (duplicates 
                  (check-for-duplicates (cons keyword-name keyword-aliases)))
            (df-parse-error
             (one-string-nl
              "In the KEYWORD clause ~S,"
              "one or more of the aliases for ~S are either identical to"
              "~S or duplicates or another alias. All the aliases of a"
              "keyword must be unique!"
              "The symbols which are not unique are: ~S")
             keyword-clause keyword-name keyword-name duplicates
             )))
    (let ((multiple-clauses? (> (length keyword-clauses) 1)))
      (vwhen (duplicates (check-for-duplicates keywords-and-aliases))
        (df-parse-error
         (one-string-nl
          (if multiple-clauses?
              "In one or more KEYWORD clauses,~A"
            "In the KEYWORD clause ~S,")
          "one or more of the specified keywords and/or their aliases have"
          "the same name!  All keywords and their aliases must be unique."
          "The duplicated names are: ~S.")
         (if multiple-clauses? "" (first keyword-clauses))
         duplicates
         )))
    (list clause)
    ))


(defun values-return-type? (x)
  (and (listp x) (symbol= :values (first x))))

(defmethod parse-df-clause ((name (eql :return)) clause)
  (let* ((data (merged-clause-data clause))
         (return-info (first data))
         (return-type (merged-data-datum return-info))
         (return-clause (merged-data-form return-info))
         (values? (values-return-type? return-type)))
    (unless (or (symbolp return-type) (listp return-type))
      (df-parse-error
       (one-string-nl
        "In the clause defining the return type, ~S,"
        "you provided '~S' as the return type, "
        "but a return type must be a valid type specifier,"
        "which means it must be either a symbol or a list."
        "It is in fact of type ~S.")
       return-clause return-type (printed-type-of return-type)))
    (let ((bad-type nil))
      (handler-case 
          (if values?
              (loop for type in (cdr return-type)
                    do
                    (setq bad-type type)
                    (subtypep type t))
            (progn
              (setq bad-type return-type)
              (subtypep return-type t)
              ))
        (error 
         (c) 
         (df-parse-error
          (one-string-nl
           "In the clause defining the return type, ~S,"
           (if values? 
               "You provided ~S as one of the return types,"
             "you provided ~S as the return type,"
             )
           "but a return type must be a valid type specifier,"
           "and ~S does not seem to be valid."
           "The actual error produced parsing this invalid"
           "type specifier was: '~A'")
          return-clause bad-type bad-type c)))
      (list clause))))

(defmethod parse-df-clause ((name (eql :type)) clause)
  (let ((type-data (merged-clause-data clause))
        (type-clauses nil)
        (args nil))
    (loop for ((arg type) type-clause) in type-data do 
          (restart-case 
              (progn
                (parse-df-argument-typespec arg type type-clause)
                (pushnew type-clause type-clauses)
                (push arg args))
            (continue-df-parse () nil)
            ))
    (vwhen (duplicates (check-for-duplicates args))
      (let ((all-clauses-same? (= (length type-clauses) 1)))
        (df-parse-error
         (one-string-nl
          (if all-clauses-same?
              "In the type specification clause ~S"
            "In multiple type specification clauses for the variables ~S,")
          "one or more of the type clause variable names is"
          "not unique!  There can be at most one type specification"
          "per DEFINE-FUNCTION variable."
          "The non-unique variables are:"
          "~S.")
         (if all-clauses-same? (first type-clauses) args)
         duplicates)))
    (list clause)))

(defmethod parse-df-clause ((name (eql :convert)) clause)
  (let ((convert-data (merged-clause-data clause))
        (convert-info nil)
        (type-clauses nil)
        (args nil))
    (flet ((oops-bad-type? (arg type type-clause direction) 
             (restart-case 
                 (handler-case 
                     (subtypep type t)
                   (error 
                    (c) 
                    (df-parse-error
                     (one-string-nl
                      "In the conversion clause ~S,"
                      "the variable ~S is being converted ~A type ~S,"
                      "but that type is not a valid type specifier."
                      "The actual error produced parsing this invalid"
                      "type specifier was: '~A'")
                     type-clause arg direction type c)))
               (continue-df-parse () nil)
               )))
      (loop for ((arg to-type from-type) type-clause) in convert-data do 
            (pushnew type-clause type-clauses)
            (push arg args)
            (oops-bad-type? arg to-type type-clause "to")
            (oops-bad-type? arg from-type type-clause "from")
            )
      (loop 
       for (convert-item type-clause) in convert-data do 
       (restart-case 
           (progn
             (vwhen (duplicate
                     (find
                      convert-item convert-info :key 'first :test 'equal))
               (let ((clauses-same? (eq type-clause (second duplicate))))
                 (df-parse-error
                  (one-string-nl
                   (if clauses-same?
                       "In the conversion clause ~S"
                     "In multiple conversion clauses for the variable ~S,")
                   "You are defining two identical conversions!  There"
                   "should be exactly one conversion for each set of"
                   "(symbol from-type to-type conversion-function-name)")
                  (if clauses-same? type-clause (first convert-item))
                  )))
             (vwhen 
                 (duplicate 
                  (find convert-item convert-info 
                        :key 'first
                        :test 
                        (lambda (item1 item2) 
                          (and (eq (first item1) (first item2))
                               (not (equal (second item1) (second item2)))
                               (equal (third item1) (third item2))
                               (eq (fourth item1) (fourth item2))))))
               (let ((clauses-same? (eq type-clause (second duplicate)))
                     (item1 convert-item)
                     (item2 (first duplicate)))
                 (df-parse-error 
                  (one-string-nl
                   (if clauses-same?
                       "In the conversion clause ~S"
                     "In multiple conversion clauses for the variable ~S,")
                   "You are defining two incompatible ways to convert"
                   "the argument ~S when it has a ~S value; one to convert"
                   "it from type ~S to type ~S, and another from the same"
                   "type to type ~S!  You can only specify one method"
                   "to convert a given argument from a particular type.")
                  (if clauses-same? type-clause (first convert-item))
                  (first item1) (third item1) (third item1) (second item1)
                  (second item2)
                  ))))
         (continue-df-parse () nil))
       (push (list convert-item type-clause) convert-info)
       )
      (list clause))))

(defmethod parse-df-clause ((name (eql :public)) clause)
  (list clause))

(defun parse-df-argument-typespec (arg type type-clause)
  (unless (or (symbolp type) (listp type))
    (df-parse-error
     (one-string-nl
      "In the clause defining the types of ~S's arguments,"
      "~S, "
      "you provided '~S' as the type specifier for ~S."
      "A type specifier must either be a symbol or a list."
      "What you provided is in fact of type ~S.")
     *df-name* type-clause type arg (printed-type-of type)))
  (handler-case 
      (subtypep type t)
    (error 
     (c) 
     (df-parse-error
      (one-string-nl
       "In the clause defining the types of ~S's arguments,"
       "~S,"
       "you provided '~S' as the type specifier for ~S."
       "But ~S does not seem to be a valid type specifier."
       "The actual error produced parsing this invalid"
       "type specifier was: '~A'")
      *df-name* type-clause type arg type c)))
  (list type-clause)
  )

(defmethod parse-df-clause ((name (eql :init)) clause)
  (let ((init-data (merged-clause-data clause))
        (init-clauses nil)
        (args nil))
    (loop for ((arg nil) init-clause) in init-data do 
          (pushnew init-clause init-clauses)
          (push arg args))
    (vwhen (duplicates (check-for-duplicates args))
      (let ((all-clauses-same? (= (length init-clauses) 1)))
        (df-parse-error
         (one-string-nl
          (if all-clauses-same?
              "In the initialization clause ~S"
            "In multiple initialization clauses for the variables ~S,")
          "one or more of the initialization clause variable names is"
          "not unique!  There can be at most one initialization clause"
          "per DEFINE-FUNCTION variable."
          "The non-unique variables are:"
          "~S.")
         (if all-clauses-same? (first init-clauses) args)
         duplicates)))
    (list clause)))

(defmethod parse-df-clause ((name (eql :description)) clause)
  (parse-all-string-df-clause clause name))

(defmethod parse-df-clause ((name (eql :author)) clause)
  (parse-all-string-df-clause clause name))

(defmethod parse-df-clause ((name (eql :example)) clause)
  (parse-all-string-df-clause clause name))

(defmethod parse-all-string-df-clause (clause clause-type)
  (let ((string-data (merged-clause-data clause)))
    (loop for (string string-clause) in string-data do
          (unless (stringp string) 
            (df-parse-error
             (one-string-nl
              "In the ~A clause beginning: ~A"
              "the ~A element ~S is not a string!"
              "All elements of a ~A clause must be strings.")
             clause-type (limited-form-string string-clause 30)
             clause-type string
             clause-type)))
    (list clause)))
             
(defmethod parse-df-clause ((name (eql :see-also)) clause)
  (let ((ref-data (merged-clause-data clause)))
    (loop for (reference refclause) in ref-data do
          (unless (or (stringp reference) (symbolp reference)) 
            (df-parse-error
             (one-string-nl
              "In the see-also clause beginning: ~A"
              "the reference ~S is neither a symbol nor a string!"
              "All elements of a see-also clause must be symbols or strings.")
             (limited-form-string refclause 30)
             reference)))
    (list clause)))
