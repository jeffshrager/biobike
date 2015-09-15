;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

(in-package :bioutils) 

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

(defun parse-loop (loop-form &key (error-limit 3) (debug? *loop-debug*))
  (block ok
    (let* ((*current-loop-form* loop-form)
           (*loop-clauses* nil)
           (*loop-errors* nil)
           (*loop-semantics* nil)
           (*loop-debug* debug?)
           )
      (block oops
        (handler-bind 
            ((loop-parse-error
              (lambda (c) 
                (push c *loop-errors*)
                (when (> (length *loop-errors*) error-limit) (return-from oops))
                (let ((loop-parse-restart (find-restart 'continue-loop-parse)))
                  (if (null loop-parse-restart)
                      (return-from oops)
                    (invoke-restart loop-parse-restart)
                    )))))
          (setq *loop-clauses* (parse-loop-into-clauses (cdr loop-form)))
          (verify-clause-constraints *loop-clauses*)
          (when *loop-debug* (pprint *loop-clauses*))
          (setq *loop-semantics* (analyze-loop-clauses *loop-clauses*))
          (when (null *loop-errors*)
            (return-from ok 
              (values (generate-loop-code *loop-semantics* loop-form) nil))
            )))
      (display-loop-errors)
      (error "LOOP syntax problems (~D of them) detected.  Cannot continue."
             (length *loop-errors*))
      )))

(defun pparse (loop-form) 
  (let ((parse (parse-loop loop-form)))
    (pprint parse)
    parse))

(defun display-loop-errors ()
  (loop for e in (reverse *loop-errors*) do
        (format t "~%Loop syntax error:~A~%" e)))

(defun parse-loop-into-clauses (loop-form &aux clause)
  (loop until (null loop-form) collect
        (progn
          (multiple-value-setq (clause loop-form) (grab-next-clause loop-form))
          clause
          )))

(defun grab-next-clause (loop-form)
  (cond 
   ((null loop-form) (values nil nil))
   ((loop-keyword? (first loop-form))
    ;; returns two values
    (parse-loop-clause (keywordize (first loop-form)) loop-form))
   (t 
    ;; no loop keyword so must be implied DO body
    (parse-implied-do-body loop-form)
    )))

(defun separate-loop-clause (type form n &optional (syntax-error nil))
  (let* ((key (form->key form))
         (ckey (key->ckey key))
         (class (key->class key)))
    (values 
     (make-loop-parse 
      :class class
      :type type
      :ckey ckey
      :key key 
      :form (subseq form 0 n)
      :syntax-ok? (not syntax-error)
      :semantics-ok? t
      :declaration nil
      :original-form (subseq form 0 n)
      )
     (nthcdr n form)
     )))

(defgeneric parse-loop-clause (key form &key (alias key))
  (:documentation
   #.(one-string-nl
      "Returns two values. The first value represents the clause parsed,"
      "and is of type LOOP-PARSE, which has a CLASS and TYPE, the subforms"
      "constituting the clause, and whether there were errors detected."
      "The second value is the remainder of FORM after the parsed"
      "clause has been extracted."
      )))

;; init j = 5
;; initialize k = (foo)


(defmethod parse-loop-clause ((key (eql :init)) form &key (alias key))
  (let ((clause-length 4))
    (oops-middle form clause-length)
    (oops-immediately-after form clause-length)
    (destructuring-bind (init symbol equal-sign value &rest goo) form
      (declare (ignore init goo value))
      (with-parse-error-restart 
          (error? (separate-loop-clause key form clause-length error?))
        (cond
         ((listp symbol) 
          (loop for s in symbol do 
                (unless (symbolp s) 
                  (error (oops-must-be-destructure-symbol form s symbol)))))
         (t 
          (unless (symbolp symbol)
            (error (oops-must-be-symbol form symbol)))))
        (unless (symbol= equal-sign :=)
          (error
           (within-loop-condition
            (one-string-nl
             "within ~A clause ~S,"
             "the token immediately after the variable ~S, ~S,"
             "must be an '=' sign, but it is not.")
            alias (loop-clause-identifier (subseq form 0 clause-length))
            symbol equal-sign
            )))))))

(defmethod parse-loop-clause ((key (eql :initialize)) form &key (alias key))
  (parse-loop-clause :init form :alias alias))

(defmethod parse-loop-clause ((key (eql :with)) form &key (alias key))
  (within-loop-warn 
   t
   (one-string-nl
    "contains a WITH clause!"
    "  A WITH clause is executed exactly once before the loop starts,"
    "  not each time through the loop (like FOR or AS clauses are)."
    "  Since WITH is easy to confuse with FOR or AS, we ask you to"
    "  use INIT or INITIALIZE instead of WITH, and if you do, you won't"
    "  see this obnoxious warning.")
   )
  (parse-loop-clause :init form :alias alias))

;; for k in ...
;; for k on ... 
;; for (i j k) in ...
;; for (i j k) on ...
;; for k = ...
;; for k from ...

(defmethod parse-loop-clause ((key (eql :for)) form &key (alias key))
  (let ((for-type (third form)))
    (cond
     ((symbol= for-type :in) (parse-loop-clause :for-in-on form :alias alias))
     ((symbol= for-type :on) (parse-loop-clause :for-in-on form :alias alias))
     ((symbol= for-type :=) (parse-loop-clause :for-= form :alias alias))
     ((symbol= for-type :from) (parse-loop-clause :for-from form :alias alias))
     ((symbol= for-type :fixnum) 
      (multiple-value-bind (clause remainder)
          (parse-loop-clause 
           :for (append (subseq form 0 2) (subseq form 3)) :alias alias)
        (setf (loop-parse-declaration clause) (list :fixnum))
        (setf (loop-parse-original-form clause) 
              (let ((clause-form (loop-parse-form clause)))
                (append (subseq clause-form 0 2) 
                        (list for-type)
                        (subseq clause-form 2)
                        )))
        (values clause remainder)
        ))
     (t 
      ;; To continue would need to find next clause-start token
      (let ((symbol (second form)))
        (when (symbolp symbol) 
          (when (or (find #\= (string symbol) :start 1) 
                    (and (symbolp for-type) 
                         (char= #\= (char (string for-type) 0))))
            (error 
             (within-loop-condition 
              (one-string-nl
               "within ~A clause ~S,"
               "the token immediately after the variable(s) ~S must be either"
               "'IN', 'ON', 'FROM' or '=' but it is not."
               "(This token, ~S, or the symbol immediately preceding"
               "this token, ~S, has an '=' in it."
               "You cannot write something like"
               "'for x= 5' or 'for x=5' or 'for x =5',"
               "you must put spaces before and after the '=' sign,"
               "e.g., 'for x = 5'.)")
              alias (loop-clause-identifier (first-n 4 form)) (second form)
              (third form) (second form)))))
        (error 
         (within-loop-condition
          (one-string-nl
           "within ~A clause ~S,"
           "the token immediately after the variable(s) ~S must be either"
           "'IN', 'ON', 'FROM' or '=' but it is not.")
          alias (loop-clause-identifier (first-n 4 form)) (second form)
          )))))))

(defmethod parse-loop-clause ((key (eql :as)) form &key (alias key))
  (parse-loop-clause :for form :alias alias))

(defmethod parse-loop-clause ((key (eql :assign)) form &key (alias key))
  (parse-loop-clause :for form :alias alias))


;; for j in list
;; for (x y) in list
;; for j in list by 'cdr
;; for (x y) in list by 'cddr
;; (and equivalent for 'on')

(defmethod parse-loop-clause 
           ((key (eql :for-in-on)) form &key (alias key) 
            &aux
            (symbol (second form)) 
            (clause-length 4) (by? nil) (on? nil) for-type)
  (declare (ignore alias))
  (when (symbol= :on (third form)) (setq on? t))
  (when (symbol= :by (nth clause-length form))
    (setq clause-length 6) 
    (setq by? t))
  (oops-middle form clause-length)
  (oops-immediately-after form clause-length)
  (with-parse-error-restart 
      (error? (separate-loop-clause for-type form clause-length error?))
    (flet ((for-symbol (symbols?) 
             (if symbols? 
                 (if on?
                     (if by? :for-symbols-on-by :for-symbols-on)
                   (if by? :for-symbols-in-by :for-symbols-in))
               (if on?
                   (if by? :for-symbol-on-by :for-symbol-on)
                 (if by? :for-symbol-in-by :for-symbol-in))
               )))
      (cond
       ((listp symbol) 
        (loop for s in (flatten symbol) do 
              (unless (symbolp s) 
                (error (oops-must-be-destructure-symbol form s symbol))))
        (setq for-type (for-symbol t)))
       (t 
        (unless (symbolp symbol)
          (error (oops-must-be-symbol form symbol)))
        (setq for-type (for-symbol nil)))
       ))))

;; for k = 5
;; for (i j) = (values 1 2)
;; for k = 5 then (1+ k)
;; for (i k) = (values 1 2) then (values 3 4)

(defmethod parse-loop-clause 
           ((key (eql :for-=)) form &key (alias key)
            &aux  
            (symbol (second form)) (clause-length 4) (then? nil) for-type)
  (declare (ignore alias))
  (when (symbol= :then (nth clause-length form)) 
    (setq clause-length 6) (setq then? t))
  (oops-middle form clause-length)
  (oops-immediately-after form clause-length)
  (with-parse-error-restart
      (error? (separate-loop-clause for-type form clause-length error?))
    (cond
     ((listp symbol) 
      (loop for s in (flatten symbol) do 
            (unless (symbolp s) 
              (error (oops-must-be-destructure-symbol form s symbol))))
      (setq for-type (if then? :for-symbols-=-then :for-symbols-=)))
     (t 
      (unless (symbolp symbol)
        (error (oops-must-be-symbol form symbol)))
      (setq for-type (if then? :for-symbol-=-then :for-symbol-=)))
     )
    (when (symbol= (fifth form) :to) (error (oops-then-not-to form)))
    ))


;; for j from 1
;; for j from 1 by k
;; for j from 1 to 10
;; for j from 1 upto 10 
;; for j from 10 downto 1
;; for j from 1 below 10
;; for j from k to m by 2
;; for j from k upto m by 2
;; for j from k downto m by 2
;; for j from k below 10 by 3

(defmethod parse-loop-clause 
           ((key (eql :for-from)) form &key (alias key)
            &aux 
            (symbol (second form)) (clause-length 4)
            (by? nil) (below? nil) (downto? nil))
  (declare (ignore alias))
  (flet ((is-by? (token) (member token '(:by :step) :test 'symbol=))
         (is-to? (token) (member token '(:to :upto :below) :test 'symbol=))
         (is-downto? (token) (symbol= token :downto))
         (is-below? (token) (symbol= token :below))
         )
    ;; Figure out the clause length, and which prepositions are present.
    (let ((token (nth clause-length form)))
      (cond
       ;; for j from 1 by k
       ((is-by? token)
        (setq clause-length 6) (setq by? t) (setq below? nil))
       ;; for j from 1 to 10 ...
       ((or (is-to? token) (is-downto? token))
        (setq clause-length 6)
        (setq below? (is-below? token))
        (setq downto? (is-downto? token))
        (let ((token2 (nth clause-length form)))
          ;; for j from 1 to 10 by 2
          (when (is-by? token2) (setq clause-length 8) (setq by? t))
          ))
       ;; for j from 1
       (t nil)
       ))
    (oops-middle form clause-length)
    (oops-immediately-after form clause-length)
    (with-parse-error-restart
        (error? 
         (separate-loop-clause
          (case clause-length
            (4 :for-symbol-from)
            (6
             (cond
              (by? :for-symbol-from-by)
              (downto? :for-symbol-from-downto)
              (below? :for-symbol-from-below)
              (t :for-symbol-from-to)
              ))
            (8 
             (cond
              (below? :for-symbol-from-below-by) 
              (downto? :for-symbol-from-downto-by)
              (t :for-symbol-from-to-by))
             ))
          form clause-length error?))
      (unless (symbolp symbol) (error (oops-must-be-symbol form symbol)))
      (when (symbol= :then (fifth form)) (error (oops-to-not-then form)))
      )))


      
;; while (< k 5)

(defmethod parse-loop-clause ((key (eql :while)) form &key (alias key))
  (declare (ignore alias))
  (parse-condition-clause form key))

;; until (> k 10)

(defmethod parse-loop-clause ((key (eql :until)) form &key (alias key))
  (declare (ignore alias))
  (parse-condition-clause form key))

;; when (x < 5)

(defmethod parse-loop-clause ((key (eql :when)) form &key (alias key))
  (declare (ignore alias))
  (parse-condition-clause form key))

;; unless (x < 5)

(defmethod parse-loop-clause ((key (eql :unless)) form &key (alias key))
  (declare (ignore alias))
  (parse-condition-clause form key))

(defun parse-condition-clause (form key)
  (let ((clause-length 2))
    (oops-middle form clause-length)
    (oops-immediately-after form clause-length)
    (separate-loop-clause key form clause-length)
    ))

;; do &rest forms <<until next loop keyword or end of loop form>>

(defmethod parse-loop-clause ((key (eql :do)) form &key (alias key))
  (declare (ignore alias))
  (let ((do-body-forms (parse-do-body (cdr form))))
    (separate-loop-clause :do form (1+ (length do-body-forms)))
    ))

(defmethod parse-loop-clause ((key (eql :body)) form &key (alias key))
  (parse-loop-clause :do form :alias alias)
  )

;; &rest forms <<until next loop keyword or end of loop form>>

(defun parse-implied-do-body (loop-form)
  ;; search until we find the next loop keyword or the end of the form
  (let ((do-body-forms (parse-do-body loop-form)))
    (separate-loop-clause :implied-do loop-form (length do-body-forms))
    ))

(defun parse-do-body (form)
  (let ((rest-of-loop-form form))
       (loop for do-body-form in rest-of-loop-form
             until (loop-keyword? do-body-form)
             collect do-body-form 
             )))

;; all the aggregation commands 

(defmacro loop-aggregation-parse-method-generator (key alias)
  `(progn
     (defmethod parse-loop-clause ((key (eql ,key)) form &key (alias key))
       (declare (ignore alias))
       (let ((clause-length 2))
         (oops-middle form clause-length)
         (separate-loop-clause key form clause-length)
         ))
     (defmethod parse-loop-clause ((key (eql ,alias)) form &key (alias key))
       (parse-loop-clause ,key form :alias alias)
       )))

(loop-aggregation-parse-method-generator :collect :collecting)
(loop-aggregation-parse-method-generator :count :counting)
(loop-aggregation-parse-method-generator :sum :summing)
(loop-aggregation-parse-method-generator :min :minimizing)
(loop-aggregation-parse-method-generator :max :maximizing)
(loop-aggregation-parse-method-generator :append :appending)
(loop-aggregation-parse-method-generator :nconc :nconcing)

;; finally form (goes to end of form)

(defmethod parse-loop-clause ((key (eql :finally)) form &key (alias key))
  (declare (ignore alias))
  (let ((clause-length (length form)))
    (separate-loop-clause key form clause-length)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-clauses-of-class (clauses class)
  (remove-if-not 
   (lambda (clause) (eq (loop-parse-class clause) class)) clauses))


(defun verify-clause-constraints (loop-clauses)

  ;; Verify that there is at least one iteration or execution clause.

  (restart-case
      (let ((iter-clauses 
             (extract-clauses-of-class loop-clauses 'loop-iterator))
            (do-clauses 
             (extract-clauses-of-class loop-clauses 'loop-execution)))
        (unless (or iter-clauses do-clauses)
          (error
           (within-loop-condition
            (one-string-nl
             "there aren't any loop control clauses (FOR, AS, WHEN or UNLESS),"
             "nor are there any statements to execute as the LOOP body."
             "Therefore if this loop executed it woudl probably never terminate"
             "and is therefore not a legal LOOP command."
             "(Use (loop while t ...) if you need to write an 'infinite' loop)" 
             )))))
    (continue-loop-parse () nil))

  ;; Verify that all the clauses appear in the correct ordering.

  (loop 
   for preceding-clause = nil then loop-clause
   for preceding-clause-form = nil then clause-form
   for preceding-clause-class = nil then clause-class
   for loop-clause in loop-clauses 
   as clause-class = (loop-parse-class loop-clause) 
   as clause-key = (loop-parse-key loop-clause)
   as clause-form = (loop-parse-form loop-clause)
   for remaining-clauses on loop-clauses
   as legal-preceding-clause-classes = (preceding-clause-classes clause-class)
   do
   (restart-case 
       (unless 
           (member preceding-clause-class legal-preceding-clause-classes)
         (error
          (ecase clause-class
            (loop-init
             (within-loop-condition
              (one-string-nl
               "the loop clause ~S"
               "appears after the clause ~S,"
               "but all initialization clauses (INIT or INITIALIZE) must appear"
               "before any other type of LOOP clause. (The initializations are"
               "executed before the loop itself is performed.)")
              (loop-clause-identifier clause-form)
              (loop-clause-identifier preceding-clause-form)
              ))
            (loop-iterator
             (within-loop-condition
              (one-string-nl
               "the loop clause ~S"
               "appears after the clause ~S,"
               "but every iteration clause (FOR, AS, WHILE or UNTIL)"
               "must appear after the initialization clauses, if any,"
               "and before all WHEN, UNLESS, DO, COLLECT, SUM, APPEND:
                and FINALLY (or other similar) clauses.")
              (loop-clause-identifier clause-form)
              (loop-clause-identifier preceding-clause-form)
              ))
            ((loop-conditional loop-execution loop-aggregator)
             (within-loop-condition
              (one-string-nl
               "the loop clause ~S"
               "appears after the ~S clause ~S,"
               "but this is illegal LOOP syntax:"
               "A ~A clause may not immediately follow a ~A clause."
               "(Every ~A clause must appear after the iteration clauses"
               "(i.e. FOR, AS, WHILE or UNLESS -- and there must be at least"
               "one such iteration clause) and before any FINALLY clause.)")
              (loop-clause-identifier clause-form)
              (loop-parse-key preceding-clause)
              (loop-clause-identifier preceding-clause-form)
              (loop-parse-key loop-clause)
              (loop-parse-key preceding-clause)
              (if clause-key (first clause-form) :body)
              ))
            (loop-terminator 
             (cond
              ((and (null preceding-clause-class) 
                    (null (cdr remaining-clauses)))
               (within-loop-condition
                (one-string-nl
                 "The FINALLY clause ~S"
                 "is not allowed to be the only clause! (the FINALLY clause"
                 "would never execute and the LOOP would never terminate).")
                (loop-clause-identifier clause-form)
                ))
              ((null (cdr remaining-clauses))
               (within-loop-condition
                (one-string-nl
                 "the FINALLY clause ~S"
                 "appears immediately after the clause"
                 "~S,"
                 "but it may only appear after the LOOP body"
                 "(or an iteration clause if there is no LOOP body).")
                (loop-clause-identifier clause-form)
                (loop-clause-identifier preceding-clause-form)
                (loop-parse-ckey preceding-clause)
                ))
              (t 
               (within-loop-condition 
                (one-string-nl
                 "the FINALLY clause ~S"
                 "does not occur as the last clause of the loop, "
                 "but it must always be the very last clause.")
                (loop-clause-identifier clause-form)
                ))
              )))))
     (continue-loop-parse () nil)
     ))

  ;; Verify there is at most one DO / IMPLIED DO clause

  (restart-case
      (let ((do-clauses 
             (extract-clauses-of-class loop-clauses 'loop-execution)))
        (when (> (length do-clauses) 1)
          (error
           (within-loop-condition
            (apply
             's+
             (one-string-nl
              "There is more than 1 code body (a block of code following a DO,"
              "or a set of statements to be executed as the LOOP body.)"
              "You can only have one block of code be executed as the body of"
              "the LOOP.  The beginnings of each multiple code block are:")
             (loop for do-clause in do-clauses
                   as clause-type = (loop-parse-type do-clause)
                   as actual-clause = (loop-parse-form do-clause)
                   collect 
                   (formatn 
                    "  ~S~%"
                    (if (eq clause-type :do)
                        (loop-clause-identifier (cdr actual-clause))
                      (loop-clause-identifier actual-clause)
                      ))))))))
    (continue-loop-parse () nil))

  ;; Verify there is at most one AGGREGATION clause

  (restart-case
      (let ((agg-clauses 
             (extract-clauses-of-class loop-clauses 'loop-aggregator)))
        (when (> (length agg-clauses) 1)
          (error
           (within-loop-condition
            (apply
             's+
             (one-string-nl
              "There's more than 1 'result gathering' clause (COLLECT, APPEND, "
              "SUM, SUMMING, MAX or other related clause), but only one such"
              "result gathering clause is allowed."
              "The multiple gathering clauses are: ~%")
             (loop for agg-clause in agg-clauses
                   as actual-clause = 
                   (loop-parse-original-form agg-clause)
                   collect 
                   (formatn "~S~%" (loop-clause-identifier actual-clause))
                   ))))))
    (continue-loop-parse () nil))

  ;; Verify that each WHEN/UNLESS clause is followed immediately
  ;; by a DO, IMPLIED DO, or aggregation clause.

  (loop for loop-clause in loop-clauses 
        as class = (loop-parse-class loop-clause)
        as form = (loop-parse-form loop-clause)
        for remaining-clauses on (cdr loop-clauses)
        when (eq class 'loop-conditional)
        do
        (restart-case
            (let ((next-clause (first remaining-clauses)))
              (when (null next-clause)
                (error
                 (within-loop-condition
                  (one-string-nl
                   "the conditional clause ~S"
                   "appears as the final LOOP clause,"
                   "but each ~A clause must appear immediately before a DO, "
                   "COLLECT, SUM, SUMMING, MAX (or similar) clause.")
                  (loop-clause-identifier form)
                  (first form)
                  )))
              (let ((next-class (loop-parse-class next-clause))
                    (next-type (loop-parse-type next-clause))
                    (next-form (loop-parse-form next-clause)))
                (declare (ignore next-type))
                (unless (or (eq next-class 'loop-execution)
                            (eq next-class 'loop-aggregator))
                  (error
                   (within-loop-condition
                    (one-string-nl
                     "the conditional clause ~S"
                     "appears immediately before the clause ~S,"
                     "but each ~A clause must only appear immediately before a"
                     "DO, COLLECT, SUM, SUMMING, MAX (or similar) clause,"
                     "not a ~A clause.")
                    (loop-clause-identifier form)
                    (loop-clause-identifier next-form)
                    (first form)
                    (first next-form)
                    )))))
          (continue-loop-parse () nil)))

  ;; Okay, it's a reasonable loop form...

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun analyze-loop-clauses (loop-clauses)
  (loop for loop-clause in loop-clauses
        as class = (loop-parse-class loop-clause)
        as type = (loop-parse-type loop-clause)
        as form = (loop-parse-form loop-clause)
        collect 
        (restart-case
            (progn
              (analyze-loop-clause-for-loop-keywords form loop-clause)
              (analyze-loop-clause class type form loop-clause))
          (continue-loop-parse
           () 
           (setf (loop-parse-semantics-ok? loop-clause) nil)
           loop-clause
           ))))

(defun analyze-loop-clause-for-loop-keywords (form loop-clause)
  (let* ((type (loop-parse-type loop-clause))
         (legal-keywords (cdr (assoc type *clause-symbols-by-type*)))
         (suspect-keywords 
          (set-difference *loop-reserved-words* legal-keywords :test 'symbol=))
         (search-form (if (eq type :implied-do) form (cdr form))) 
         (keys (intersection suspect-keywords search-form :test 'symbol=)))
    (when keys
      (within-loop-warn
       nil
       (one-string-nl
        "the clause ~A"
        (one-string "contains the symbol~p " *english-and-list* ".")
        "These symbols have special meaning to LOOP as marking the beginning"
        "of clauses or as instructions within a clause."
        "This could indicate that the code you wrote is not doing"
        "what you might think it is. Please examine it.  If your code"
        "is correct (because you really have a symbol or symbols which"
        (one-string "have the same name as " *english-or-list* "),")
        "you can make this warning go away by changing their names.")
       (loop-clause-identifier form)
       (length keys)
       keys
       keys
       ))))

(defun loop-by-function? (x)
  (or (member x '(cdr cddr cdddr cddddr))
      (and (listp x)
           (or (symbolp (unquote x))
               (eq (first x) 'lambda)
               (eq (first x) 'function)
               ))))

(defgeneric analyze-loop-clause (class type form clause))

(defmethod analyze-loop-clause ((class t) type form clause)
  (declare (ignore type form))
  clause)

(defmethod analyze-loop-clause 
           ((class (eql 'loop-iterator)) (type t) form clause)
  (ecase type
    ((:for-symbol-in :for-symbols-in)
     ;; for j in <something>
     (verify-constant-iteration-object (fourth form) form))
    ((:for-symbol-on :for-symbols-on)
     (verify-constant-iteration-object-on-list (fourth form) form))
    ((:for-symbol-in-by :for-symbol-on-by :for-symbols-in-by :for-symbols-on-by)
     ;; for (x y z) in <something> by 'cdr
     (verify-constant-iteration-object-list (fourth form) (sixth form) form)
     (verify-iteration-by-list-function (sixth form) form))
    (:for-symbol-from
     ;; for j from 10
     (verify-iteration-number (fourth form) form))
    (:for-symbol-from-by
     ;; for j from 5 by -1
     (verify-iteration-number (fourth form) form)
     (verify-iteration-number (sixth form) form))
    ((:for-symbol-from-to :for-symbol-from-downto :for-symbol-from-below)
     ;; for j from 1 to 10
     ;; for j from 1 below 20
     (analyze-loop-clause-to-downto-or-below form))
    ((:for-symbol-from-to-by :for-symbol-from-downto-by 
      :for-symbol-from-below-by)
     ;; for j from 5 to -1 by 3
     ;; for j from 5 below 10 by 2
     (analyze-loop-clause-to-downto-or-below-by form))
    ((:for-symbol-= :for-symbol-=-then :for-symbols-= :for-symbols-=-then) 
     ;; for j = 5
     ;; for j = 5 then (1+ j)
     nil)
    ((:while :until) nil)
    )
  clause)
  
(defmethod analyze-loop-clause 
           ((class (eql 'loop-execution)) (type (eql :do)) form clause)
  (verify-no-atoms-in-do-body form clause)
  (when (= (length form) 1)
    (within-loop-warn 
     nil
     (one-string-nl
      "The DO clause has no statements in it!"  
      "This could indicate that the code you wrote is not doing"
      "what you might think it is. Please examine it. You can make this warning"
      "go away by removing either the 'DO' or adding a statement to be executed"
      "as the DO body immediately after the 'DO'.")
     ))
  clause)

(defmethod analyze-loop-clause 
           ((class (eql 'loop-execution)) (type (eql :implied-do)) form clause)
  (verify-no-atoms-in-do-body form clause)
  clause)

(defun analyze-loop-clause-to-downto-or-below (form)
  ;; for j from 5 to 10
  ;; for j from 5 downto 1
  ;; for j from 5 below 10
  (let ((from (fourth form)) (verb (fifth form)) (to (sixth form)))
    (verify-iteration-number from form)
    (verify-iteration-number to form)
    (when (and (constantp from) (constantp to))
      (verify-iteration-constant-bounds 
       (unquote from) (unquote to) (keywordize verb) nil form)
      )))

(defmethod analyze-loop-clause-to-downto-or-below-by (form)
  ;; for j from 5 to -1 by 3
  ;; for j from 5 below 10 by 2
  (let ((from (fourth form)) 
        (verb (fifth form))
        (to (sixth form))
        (by (eighth form)))
    (verify-iteration-number from form)
    (verify-iteration-number to form)
    (verify-iteration-number by form)
    (when (and (constantp from) (constantp to) (constantp by))
      (verify-iteration-constant-bounds 
       (unquote from) (unquote to) (keywordize verb) (unquote by) form)
      )))
