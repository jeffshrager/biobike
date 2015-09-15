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

;;; Verification phase 

(defun verify-df-clauses (pclauses)
  (let ((verified-clauses 
         (loop for clause in pclauses 
               as clause-name = (first clause)
               collect
               (restart-case 
                   (verify-df-clause clause-name clause pclauses)
                 (continue-df-parse () nil)))))
    (unless (find :body pclauses :key 'first)
      (df-parse-error 
       (one-string-nl
        "The function definition has no body!"
        "All ~A definitions must have an explicit"
        "body, even if that body is NIL.")
       *df-type*))
    verified-clauses
    ))

;;; First, routines to extract information about the required, token,
;;; key and local arguments.

(defun find-df-clause (clause-keyword clauses)
  (find clause-keyword clauses :key 'first))

(defun find-df-clause-data (clause-keyword clauses)
  (second (find-df-clause clause-keyword clauses)))

(defun required-clause-data (clauses)
  (find-df-clause-data :required-args clauses))

(defun df-required-arg? (arg clauses)
  (member arg (required-clause-data clauses) :key 'first))

(defun required-arg-clause (arg clauses)
  (second (find arg (required-clause-data clauses) :key 'first)))

(defun all-required-args (clauses)
  (mapcar 'first (required-clause-data clauses)))



(defun token-clause-data (clauses) 
  (find-df-clause-data :token-args clauses))

(defun df-token-arg? (arg clauses)
  (loop for token-datum in (token-clause-data clauses) do 
        (when (member arg (first token-datum))
          (return token-datum))))

(defun token-arg-clause (arg clauses)
  (second (df-token-arg? arg clauses)))

(defun all-token-args (clauses)
  (apply 'append (mapcar 'first (token-clause-data clauses)))) 


(defun flag-clause-data (clauses) 
  (find-df-clause-data :flag clauses))

(defun df-flag-arg? (arg clauses &optional (test 'eq))
  (loop for flag-datum in (flag-clause-data clauses) do 
        (when (funcall test arg (first flag-datum))
          (return flag-datum))))

(defun flag-arg-clause (arg clauses)
  (second (df-flag-arg? arg clauses)))

(defun all-flag-args (clauses)
  (mapcar 'first (flag-clause-data clauses))) 


(defun keyword-clause-data (clauses) 
  (find-df-clause-data :keyword clauses))

(defun df-keyword-arg? (arg clauses &optional (test 'eq))
  (loop for keyword-datum in (keyword-clause-data clauses) do 
        (when (funcall test arg (caar keyword-datum))
          (return keyword-datum))))

(defun df-keyword-arg-or-alias? (arg clauses &optional (test 'eq))
  (loop for keyword-datum in (keyword-clause-data clauses) do 
        (when (or (funcall test arg (caar keyword-datum))
                  (member arg (second (first keyword-datum)) :test test))
          (return keyword-datum))))

(defun df-alias-keyword (arg clauses &optional (test 'eq))
  (loop for keyword-datum in (keyword-clause-data clauses) do 
        (when (or (funcall test arg (caar keyword-datum))
                  (member arg (second (first keyword-datum)) :test test))
          (return (list arg (caar keyword-datum))))))

(defun keyword-arg-clause (arg clauses)
  (second (df-keyword-arg? arg clauses)))

(defun keyword-arg-or-alias-clause (arg clauses)
  (second (df-keyword-arg-or-alias? arg clauses)))

(defun all-keyword-args (clauses)
  (mapcar 'caar (keyword-clause-data clauses)))

(defun all-keyword-args-and-aliases (clauses)
  (loop for keyword-datum in (keyword-clause-data clauses) 
        append
        (cons (caar keyword-datum) (second (first keyword-datum)))))
        

        
(defun local-clause-data (clauses) 
  (find-df-clause-data :init clauses))

(defun df-local-arg? (arg clauses)
  (member arg (local-clause-data clauses) :key 'caar))

(defun local-arg-clause (arg clauses)
  (second (find arg (local-clause-data clauses) :key 'caar)))

(defun all-local-args (clauses)
  (mapcar 'caar (local-clause-data clauses)))



(defun contains-equal-sign? (symbol)
  (position #\= (string symbol)))
  

;;; The methods of the generic function VERIFY-DF-CLAUSE
;;; that semantically check each clause.

(defgeneric verify-df-clause (name clause clauses)
  (:documentation 
   #.(one-string-nl
      "Verifies that each clause is semantically correct as far as it is"
      "possible to do at macroexpansion time.  (For instance, we check"
      "that a variable used in a TYPE clause is in fact one of the function's"
      "variables that is allowed to be typed, but we don't check that"
      "a symbol named in a SEE-ALSO clause is in fact fbound.")))

(defmethod verify-df-clause ((name t) clause clauses)
  (declare (ignore clauses))
  clause)

(defmethod verify-df-clause ((name (eql :required)) clause clauses)
  (let* ((tokens (all-token-args clauses))
         (keywords (all-keyword-args clauses))
         (keywords-and-aliases (all-keyword-args-and-aliases clauses))
         (keyword-aliases (set-difference keywords-and-aliases keywords))
         (locals (all-local-args clauses))
         (requireds (all-required-args clauses)))
    
    (verify-no-duplicates 
     clauses requireds tokens :required :token :required :required  
     'required-arg-clause 'token-arg-clause)
    (verify-no-duplicates 
     clauses requireds keywords :required :keyword :required :keyword
     'required-arg-clause 'keyword-arg-clause)
    (verify-no-duplicates 
     clauses requireds keyword-aliases
     :required :keyword-alias :required :keyword
     'required-arg-clause 'keyword-arg-or-alias-clause)
    (verify-no-duplicates 
     clauses requireds locals :required :init :required :init  
     'required-arg-clause 'local-arg-clause)

    (verify-no-duplicates 
     clauses tokens keywords :token :keyword :required :keyword
     'token-arg-clause 'keyword-arg-clause)
    (verify-no-duplicates 
     clauses tokens keyword-aliases
     :token :keyword-alias :required :keyword
     'token-arg-clause 'keyword-arg-or-alias-clause)
    (verify-no-duplicates 
     clauses tokens locals :token :init :required :init  
     'token-arg-clause 'local-arg-clause)

    clause))
    
(defmethod verify-df-clause ((name (eql :keyword)) clause clauses)
  (let* ((keywords (all-keyword-args clauses))
         (keywords-and-aliases (all-keyword-args-and-aliases clauses))
         (keyword-aliases (set-difference keywords-and-aliases keywords))
         (locals (all-local-args clauses)))
    (verify-no-duplicates 
     clauses keywords locals :keyword :init :keyword :init
     'keyword-arg-clause 'local-arg-clause)
    (verify-no-duplicates 
     clauses keyword-aliases locals
     :keyword-alias :init :keyword :init
     'keyword-arg-or-alias-clause 'init-arg-clause)
    clause))

(defmethod verify-df-clause ((name (eql :init)) clause clauses)
  (declare (ignore clauses))
  clause)

(defun verify-no-duplicates
       (clauses symbol-list-1 symbol-list-2 argtype-1 argtype-2
               clause-type-1 clause-type-2 clause-function-1 clause-function-2)
  (vwhen (dups (intersection symbol-list-1 symbol-list-2))
    (loop 
     for dup in dups
     as first-clause = (funcall clause-function-1 dup clauses)
     as dup-clause = (funcall clause-function-2 dup clauses)
     do 
     (restart-case 
         (df-parse-error 
          (one-string-nl
           "The ~A argument ~S "
           "in the ~A clause ~S, "
           "has the same name as the ~A argument ~S"
           "in the ~A clause ~S -- "
           "~A and ~A argument names must be unique!"
           "(All required, token, keyword, and init args must be unique.)")
          argtype-1 dup clause-type-1 first-clause argtype-2 dup clause-type-2
          dup-clause argtype-1 argtype-2)
       (continue-df-parse () nil)
       ))))

(defmethod verify-df-clause ((name (eql :type)) clause clauses)
  (loop 
   for data in (merged-clause-data clause) 
   as datum = (merged-data-datum data)
   as original-clause = (merged-data-form data)
   as (arg nil) = datum 
   do
   (restart-case 
        (let ((argtype (which-df-argtype arg clauses)))
          (when (member argtype '(:flag :token))
            (df-parse-error
             (one-string-nl
              "In the TYPE clause ~S,"
              "(which defines types for ~S's arguments),"
              "you are specifying a type for the symbol ~S,"
              "which is a ~A type argument."
              "Only required, keyword, or local arguments can have their"
              "types specified.")
             original-clause *df-name* arg argtype))
         (unless (member argtype '(:required :keyword :local))
           (df-parse-error
            (one-string-nl
             "In the TYPE clause ~S,"
             "(which defines types for ~S's arguments),"
             "you are specifying a type for the symbol ~S,"
             "but ~S is not one of ~S's required, keyword, or local arguments."
             "Only these kind of arguments can have their types specified."
             "(Perhaps you misspelled the name?)")
            original-clause *df-name* arg arg *df-name*)))
     (continue-df-parse () nil)))
  clause)
        
(defmethod verify-df-clause ((name (eql :convert)) clause clauses)
  (loop for data in (merged-clause-data clause) 
        as datum = (merged-data-datum data)
        as original-clause = (merged-data-form data)
        as (arg nil) = datum 
        do
        (restart-case
            (let ((argtype (which-df-argtype arg clauses)))
              (when (member argtype '(:local :flag :token))
                (df-parse-error
                 (one-string-nl
                  "In the CONVERT clause ~S, "
                  "you have specified a conversion for the ~A"
                  "variable ~S.  ~A variables are not allowed to have"
                  "conversions specified; conversions are only allowed for"
                  "required and keyword arguments.")
                 original-clause argtype arg argtype))
              (unless (member argtype '(:required :keyword))
                (df-parse-error
                 (one-string-nl
                  "In the CONVERT clause ~S,"
                  "you are specifying a conversion for the symbol ~S,"
                  "but ~S is not one of ~S's required or keyword arguments."
                  "Only these kind of arguments can have conversions specified."
                  "(Perhaps you misspelled the name?)")
                 original-clause arg arg *df-name*)))
          (continue-df-parse () nil)))
  clause)

(defmethod verify-df-clause ((name (eql :mapcar)) clause clauses)
  (verify-mapping-clause name clause clauses))

(defmethod verify-df-clause ((name (eql :mapcarnn)) clause clauses)
  (verify-mapping-clause name clause clauses))

(defmethod verify-df-clause ((name (eql :maptree)) clause clauses)
  (verify-mapping-clause name clause clauses))

(defmethod verify-df-clause ((name (eql :crossmap)) clause clauses)
  (loop 
   for data in (merged-clause-data clause) 
   as arg = (merged-data-datum data)
   as original-clause = (merged-data-form data)
   do
   (restart-case 
       (let ((argtype (which-df-argtype arg clauses)))
         (when (member argtype '(:flag :token :keyword :local))
           (df-parse-error
            (one-string-nl
             "In the CROSSMAP clause ~S,"
             "you are specifying a crossmap using the variable ~S,"
             "which is a ~A type argument." 
             "Only required arguments can be crossmapped!")
            original-clause arg argtype))
         (unless (member argtype '(:required))
           (df-parse-error
            (one-string-nl
             "In the CROSSMAP clause ~S,"
             "you are specifying a crossmap using the symbol ~S,"
             "but ~S is not one of ~S's required arguments."
             "Only required arguments can be crossmapped!"
             "(Perhaps you misspelled the name?)")
            original-clause arg arg *df-name*)))
     (continue-df-parse () nil)))
  clause)

;; Maybe check that there isn't a different symbol by the same name
;; already exported from the package?  (Recall bug inquiry to Franz
;; 12/20/05) 
(defmethod verify-df-clause ((name (eql :export-from)) clause clauses)
  (declare (ignore clauses))
  (loop  
   for data in (merged-clause-data clause) 
   as package = (merged-data-datum data)
   as original-clause = (merged-data-form data)
   do
   (restart-case 
       (unless (find-package package) 
         (df-parse-error
          (one-string-nl
           "In the EXPORT-FROM clause ~S,"
           "you specify that ~S is to be exported from the"
           "~S package, but that package does not exist!")
          original-clause *df-name* package))
     (continue-df-parse () nil)))
  clause)


(defun verify-mapping-clause (name clause clauses)
  (loop 
   for data in (merged-clause-data clause) 
   as arg = (merged-data-datum data)
   as original-clause = (merged-data-form data)
   do
   (restart-case 
        (let ((argtype (which-df-argtype arg clauses)))
          (when (member argtype '(:flag :token :keyword :local))
            (df-parse-error
             (one-string-nl
              "In the ~A clause ~S,"
              "you are specifying a mapping over the variable ~S,"
              "which is a ~A type argument."
              "Only required arguments can be mapped over!")
             name original-clause arg argtype))
          (unless (member argtype '(:required))
           (df-parse-error
            (one-string-nl
             "In the ~A clause ~S,"
             "you are specifying a mapping over the variable ~S,"
             "but ~S is not one of ~S's required arguments."
             "Only required arguments can be mapped over!"
             "(Perhaps you misspelled the name?)")
            name original-clause arg arg *df-name*)))
     (continue-df-parse () nil)))
  clause)

(defun which-df-argtype (arg clauses)
  (cond 
   ((df-local-arg? arg clauses) :local)
   ((df-flag-arg? arg clauses) :flag)
   ((df-token-arg? arg clauses) :token)
   ((df-required-arg? arg clauses) :required)
   ((df-keyword-arg? arg clauses) :keyword)
   (t :unknown)
   ))



