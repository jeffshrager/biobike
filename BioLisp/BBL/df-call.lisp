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

(define-condition df-call-error (error) 
  ((explanation :initarg :explanation :reader explanation))
  (:report
   (lambda (condition stream)
     (format stream "~A" (explanation condition))
     )))

(defun dfce (format-string &rest format-args)
  (error
   (make-condition
    'df-call-error
    :explanation (apply 'format nil format-string format-args)
    )))

(DEFUN wrong-flag-advice ()
   (IF (FORWARD-FUNCALL 'CALLED-FROM-VPL)
       (S+ "You may be using an old version of "
	       "the function. Try bringing "
           "down a new copy. ")
       "Spelling error? "))

(defun verify-df-call (function-name call-form df-clauses &KEY expandable) 
  
  (let* ((required-data (find-df-clause-data :required df-clauses))
         (token-args 
          (apply 
           'append
           (mapcar 'car (find-df-clause-data :token-args df-clauses))))
         (keyword-data (find-df-clause-data :keyword df-clauses))
         (flag-data (find-df-clause-data :flag df-clauses))
         (flag-args (mapcar 'car flag-data))
         (df-name (first call-form))
         (call-args (rest call-form))
         (n-required-args 0)
         (n-token-args 0)
         (required-count 0)
         (token-count 0)
         (required-forms nil)
         (token-forms nil)
         (no-required-init nil)
         (flag-forms nil)
         (keywords-used nil)
         (actual-keywords nil)
         (keyword-values nil)
         (required-args (all-required-args df-clauses)))
          
    (loop for (arg-spec nil) in required-data do
          (cond
           ((symbolp arg-spec) (incf n-required-args))
           ((listp arg-spec) (incf n-token-args))
           (t (error
               "Internal error. arg-spec is neither a symbol nor a list."))))

    (setq 
     no-required-init 
     (formatn
      (serr+
       problem "Invalid form:"
       indent "~S"
       indent "~S requires ~D ~A")
      call-form df-name  n-required-args
       (if (= 1 n-required-args) " argument." " arguments.")
      ))     
                
    ;; Go through all the required (including token) arguments and make sure
    ;; there are appropriate matches in the calling form 

    (loop for (arg-spec nil) in required-data
          do 
          (flet ((token-args-string ()
                   (if (zerop token-count)
                       ""
                     (formatn 
                      "~%              (along with ~D token argument~P, ~S)"
                      token-count token-count (reverse token-forms)
                      ))))
            (cond
             ;; This tests for being a required argument 
             ((symbolp arg-spec)
              (unless call-args
                (cond
                 (token-forms  ; ***** Presume req'd arg misinterpretted as token
                  (PUSH (POP token-forms) call-args)
                  (DECF token-count))
                 ((zerop required-count) 
                  (dfce 
                   (one-string
                    no-required-init 
                    (serr+
                     indent "No argument was given!"
                     advice "Add the appropriate argument~P after ~S."
                     indent "Required argument~P: ~A. ~A"
                     help~A)) 
                   (length required-args) df-name  
                   (length required-args) required-args
                   (if (zerop token-count)
                       ""
                     (formatn 
                      "~%              (It includes ~D token argument~P.)"
                      token-count token-count))
                   df-name 
                   ))
                 ((= 1 required-count) 
                  (dfce 
                   (one-string
                    no-required-init
                    (serr+
                     indent "Only 1 required argument was given: ~S~A."
                     advice "Use the appropriate number of arguments."
                     indent "Required argument~P: ~A."
                     help~A)) 
                   (first (reverse required-forms)) 
                   (token-args-string) (length required-args) 
                   required-args df-name))
                 (t 
                  (dfce
                   (one-string
                    no-required-init
                    (one-string-nl
                     "But the call to ~S only has ~D required values"
                     "(the forms ~S)~A."))
                   df-name required-count (reverse required-forms)
                   (token-args-string)))
                 ))
              (incf required-count)
              (push (first call-args) required-forms)
              (pop call-args)
              )
             ;; here we process token args
             (t 
              (when call-args 
                (loop for token-arg in arg-spec do
                      (when (symbol= token-arg (first call-args))
                        (incf token-count)
                        (push (first call-args) token-forms)
                        (pop call-args)
                        (return)))))
             )))

    ;; make sure that there aren't more arguments in the call than 
    ;; are defined if there are no flag or keyword arguments to handle

    (when (and (null keyword-data) (null flag-data))
     (when call-args 
      (IF expandable ; ***********
        (PROGN
           (SETQ required-forms (APPEND (REVERSE call-args) required-forms))
           (SETQ call-args NIL))           ; ******
        (dfce
         (one-string
          no-required-init
          (serr+
           indent "Extra argument~P given: ~S. ~A"
           advice "Use the appropriate number of arguments."
           indent "Required argument~P: ~A."
           help~A))
         (length call-args) call-args
         (if (zerop n-token-args)
             ""
           (formatn 
            "~%              (Perhaps you misspelled a token argument?)"))
         (length required-args) required-args
         df-name   
           ))))

    ;; check that the flag and keyword arguments and values
    ;; provided correspond to the define-function definition
    
    (when (or keyword-data flag-data)
      (loop with rest-of-call = call-args 
            until (null rest-of-call)
            as next = (first rest-of-call)
            do 
            (unless (OR (symbolp next) expandable)  ; ***********
              (dfce
               (serr+
                problem "Invalid form:"
                indent "~S"
                indent "~S is in a position where a KEYWORD or FLAG is expected."
;               advice "Spelling error? With ~S, you may use as valid "
                advice (WRONG-FLAG-ADVICE)
                indent "With ~S, you may use as valid "
                indent "  KEYWORD~:@(~P~): ~A."
                indent "  or FLAG~:@(~P~): ~A."
                help~A)
               call-form next df-name
               (length (all-keyword-args-and-aliases df-clauses)) 
               (all-keyword-args-and-aliases df-clauses)
               (length (all-flag-args df-clauses)) 
               (all-flag-args df-clauses) df-name
               ))
            (cond
             ((df-flag-arg? next df-clauses 'symbol=)
              (push next flag-forms)
              (pop rest-of-call))
             ((df-keyword-arg-or-alias? next df-clauses 'symbol=)
              (pop rest-of-call)
              (unless rest-of-call 
                (dfce
                 (serr+
                  problem "Invalid form:"
                  indent "~S"
                  indent "~S is a KEYWORD that must be followed by a value."
                  advice "Add an appropriate value after ~S."
                  help~A)
                 call-form next next df-name
                 ))
              (let ((keyword-value (pop rest-of-call)))
                (let ((alias-and-keyword
                       (df-alias-keyword next df-clauses 'symbol=)))
                  (push alias-and-keyword keywords-used)
                  (push keyword-value keyword-values)
                  )))
             (t
              (IF expandable ; *******
                  (PROGN
                     (push (first rest-of-call) required-forms)
                     (pop rest-of-call))  ; *******              
              (dfce
               (serr+
                problem "Invalid form:"
                indent "~S"
                indent "~S is in a position where a KEYWORD or FLAG is expected."
;               advice "Spelling error? With ~S, you may use as valid "
                advice (WRONG-FLAG-ADVICE)
                indent "With ~S, you may use as valid "
                indent "  KEYWORD~:@(~P~): ~A."
                indent "  or FLAG~:@(~P~): ~A."
                help~A) 
               call-form next df-name
               (length (all-keyword-args-and-aliases df-clauses)) 
               (all-keyword-args-and-aliases df-clauses)
               (length (all-flag-args df-clauses)) 
               (all-flag-args df-clauses) df-name
               ))))))

    ;; check that the call does not contain multiple references
    ;; to flag variables

    (vwhen (duplicates (check-for-duplicates flag-forms))
      (loop 
       for dup in duplicates 
       do 
       (dfce
        (serr+
         problem "Invalid form:"
         indent "~S"
         indent "The FLAG ~S is used more than once."
         advice "Delete the duplicated FLAG: ~S."
         indent "Use another valid FLAG for ~S:"
         indent "~A."
         help~A) 
        call-form dup dup df-name (all-flag-args df-clauses) df-name)
       ))

    ;; check that the call does not contain multiple references
    ;; to keyword variables or their aliases

    (vwhen (duplicates 
            (find-duplicates 
             keywords-used :key 'second :return-exactly-one-duplicate? t))
      (loop 
       for (alias key) in duplicates 
       as other-uses = (remove-if-not
                        (lambda (x) (eq x key)) keywords-used :key 'second)
       as other-names = (mapcar 'first (remove-if
                                        (lambda (x) (eq x alias))
                                        other-uses 
                                        :key 'first
                                        ))
       do 
       (dfce
        (serr+
         problem "Invalid form:"
         indent "~S"
         indent "The KEYWORD ~S is used more than once."
         advice "Delete the duplicated KEYWORD ~S."
         indent "Replace the duplicated KEYWORD ~S by another valid KEYWORD for ~S:"
         indent "~A."
         indent "Replace the duplicated KEYWORD ~S by a valid FLAG for ~S:"
         indent "~A. ~A"
         help~A) 
        call-form  alias alias alias df-name (all-keyword-args df-clauses) alias df-name  (all-flag-args df-clauses)
        (if (null other-names)
            ""                                               
          (formatn "~%              (The other aliases used are ~S.)"
                   (remove-duplicates other-names)))
         df-name
       )))


    ;; generate the actual call form to the auxiliary function 
    ;; for this call to the define-function form

    (setq required-forms (reverse required-forms))
    (setq actual-keywords (reverse (mapcar 'second keywords-used)))
    (setq keyword-values (reverse keyword-values))

    `(,function-name 
      ,@required-forms 
      ,@(loop for token in token-args collect
              (if (member token token-forms :test 'symbol=) t nil))
      ,@(loop for flag in flag-args collect
              (if (member flag flag-forms :test 'symbol=) t nil))
      ,@(loop for keyword in actual-keywords 
              for value in keyword-values
              append 
              (list (keywordize keyword) value)
              ))

    ))

