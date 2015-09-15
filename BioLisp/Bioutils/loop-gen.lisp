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

;; (defvar *biobike-loop?* nil)

(defvar *iter-first-iteration?-var* nil)
(defvar *iter-first-agg?-var* nil)
(defvar *iter-return-var* nil)
(defvar *iter-aggregation-bindings* nil)
(defvar *iter-iterator-clauses-and-bindings* nil)

(defun not-in-bbl-form-processor? ()
  (or (not (find-package :bbi)) 
      (not (symbol-value-in-package :*in-bbl-form-processor* :bbi))))


(defun loop-clause-start-string (form &optional (length 50)) 
  (limited-form-string 
   form length 
   :format-mode "~S" :print-pretty t :single-line? t :strip-indentation? t
   ))

(defun generate-loop-code (loop-clauses original-form)
  (verify-no-duplicate-loop-vars loop-clauses)
  (when (and (null *safety*)
             (not-in-bbl-form-processor?)
             ;; (not *biobike-loop?*)
             (all-clauses-lisp-loop-kosher? loop-clauses))
    (return-from generate-loop-code 
      (render-loop-code-as-lisp-loop loop-clauses)))
  (let* ((loop-tag (gensym "LOOP-TAG-"))
         (*iter-first-iteration?-var* (gensym "FIRST-ITER-"))
         (*iter-first-agg?-var* (gensym "FIRST-AGG-"))
         (initial-bindings (initial-loop-bindings loop-clauses))
         (initial-declarations (initial-loop-declarations loop-clauses))
         (binding-variables (mapcar 'car initial-bindings))
         (other-bindings (loop-other-bindings loop-clauses))
         (*iter-return-var* (caar other-bindings))
         (*iter-aggregation-bindings* other-bindings)
         (*iter-iterator-clauses-and-bindings*
          (initial-iterator-bindings loop-clauses))
         (iterator-bindings 
          (mapcar 'second *iter-iterator-clauses-and-bindings*))
         (body-and-aggregation-forms
          (loop-execution-and-aggregation-forms loop-clauses))
         (finally-forms (loop-finally-forms loop-clauses))
         (counter (gensym "COUNTER-"))
         )
    `(let* ,(append 
             (when *safety* 
               `((*current-loop-form* ,(loop-clause-start-string original-form))
                 (*current-loop-key* :initializations)
                 ;; (*current-loopvar* nil)
                 (*current-loop-clause* nil)
                 (,counter 0)))
             (remove-if 
              'null 
              (append 
               (maybe-bbl-process-bindings initial-bindings)
               other-bindings)))
       ,@(when *safety* `((declare (fixnum ,counter))))
       ,@initial-declarations
       ,@(unless *safety* 
           `((declare (optimize (speed 3) (safety 0) (debug 0)))))
       (block nil
         (,(if *safety* 'handler-case 'progn)
          (let ((,*iter-first-iteration?-var* t)
                (,*iter-first-agg?-var* t)
                ,@(maybe-bbl-process-bindings iterator-bindings)
                ,@(maybe-bbl-loop-process-level-tracking))
            (declare 
             (ignorable ,*iter-first-iteration?-var* ,*iter-first-agg?-var*))
            ;; ,@(when *safety* `((setq *current-loopvar* nil)))
            (loop named ,loop-tag 
                  do
                  (unless (and ,@(maybe-bbl-process-forms 
                                  (iterator-steps loop-clauses) 
                                  binding-variables))
                    (progn ,@(maybe-bbl-process-forms 
                              (loop-finally-code finally-forms)
                              binding-variables)))
                  ,@(maybe-bbl-process-forms
                     body-and-aggregation-forms binding-variables)
                  (setq ,*iter-first-iteration?-var* nil)
                  ,@(when *safety* `((incf ,counter)))
                  ))
          ,@(loop-error-detection-code counter)
          )))))

(defun maybe-bbl-process-bindings (bindings)
  (if (not-in-bbl-form-processor?)
      ;; (if (not *biobike-loop?*)
      bindings
    (forward-package-funcall :bbi :bbl-process-loop-bindings bindings)
    ))


(defun maybe-bbl-process-forms (forms vars)
  (if (not-in-bbl-form-processor?) 
      ;; (if (not *biobike-loop?*)
      forms
    (forward-package-funcall :bbi :bbl-process-loop-forms forms vars)
    ))

(defun maybe-bbl-loop-process-level-tracking ()
  (if (not-in-bbl-form-processor?) 
      nil
    (forward-package-funcall :bbi :bbl-loop-level-tracking-binding)
    ))

(defun loop-finally-code (finally-forms)
  `(,@(when (and *safety* finally-forms) 
        `((setq *current-loop-key* :finally) 
          (setq *current-loop-clause* 
                ,(loop-clause-start-string `(finally ,@finally-forms)))))
    ,@finally-forms
    (return ,*iter-return-var*)
    ))

(defun make-nice-loop-time-msg (counter c)
  (list 
   (if (<= counter 100)
       (one-string
        "Before finishing the ~:R iteration of the loop, "
        "an error was detected: ~%~A")
     (one-string 
      "Before finishing iteration number ~D of the loop, "
      "an error was detected: ~%~A"))
   (1+ counter) c))

(defun loop-error-detection-code (counter)
  (when *safety* 
    `((loop-runtime-error (c) (error c))
      (error 
       (c)
       (error (apply 'within-loop-runtime (make-nice-loop-time-msg ,counter c)))
       ))))

(defun verify-no-duplicate-loop-vars (loop-clauses)
  (let* ((loop-vars 
          (remove-if 
           'null (mapcar 'first (initial-loop-bindings loop-clauses))))
         (duplicates (find-duplicates loop-vars)))
    (when duplicates 
      (oops-within-loop
       (one-string-nl
        "The initialization and iteration clauses contain duplicated variables!"
        (one-string "The duplicated variables are " *english-and-list* ".")
        "All the initialization and iteration clauses of a LOOP, taken"
        "together, must specify unique variable names.")
       duplicates))))

(defun loop-other-bindings (loop-clauses)
  (let ((aggregation-clauses 
         (extract-clauses-of-class loop-clauses 'loop-aggregator)))
    (when aggregation-clauses 
      (unless (= 1 (length aggregation-clauses))
        (error "Internal error.  More than one aggregation clause!"))
      (let* ((ac (first aggregation-clauses))
             (ckey (loop-parse-ckey ac))
             (return-binding 
              (list 
               (gensym "LOOP-RETURN-")
               (ecase ckey 
                 ((:sum :count) 0)
                 ((:max :min) nil)
                 ((:append :nconc) nil)
                 ((:collect) nil)
                 )))
             (aux-binding 
              (case ckey
                (:collect (list (gensym "COLLECT-AUX-") nil))
                (:nconc (list (gensym "NCONC-AUX-") nil))
                (:append (list (gensym "APPEND-AUX-") nil))
                (otherwise nil)
                )))
        (if aux-binding (list return-binding aux-binding) (list return-binding))
        ))))
           
        
(defun initial-loop-bindings (loop-clauses)
  (loop for clause in loop-clauses 
        as class = (loop-parse-class clause)
        as type = (loop-parse-type clause)
        as ckey = (loop-parse-ckey clause)
        as form = (loop-parse-form clause)
        append
        (loop-clause-binding class type ckey form clause)
        ))

(defun initial-loop-declarations (loop-clauses)
  (loop for clause in loop-clauses 
        as class = (loop-parse-class clause)
        as type = (loop-parse-type clause)
        as ckey = (loop-parse-ckey clause)
        as form = (loop-parse-form clause)
        append
        (loop-clause-declaration class type ckey form clause)
        ))


(defmethod loop-clause-binding ((class t) (type t) (ckey t) form clause)
  (declare (ignore form clause))
  nil)

(defmethod loop-clause-binding 
           ((class (eql 'loop-init)) (type t) (ckey t) form clause)
  (declare (ignore clause))
  (destructuring-bind (with-vars equal-sign value-form) (cdr form)
    (declare (ignore equal-sign))
    (cond
     ((symbolp with-vars) (list (list with-vars value-form)))
     ((listp with-vars) 
      (let ((with-var-symbol (gensym "WITH-BINDING-")))
        `((,with-var-symbol ,value-form)
          ,@(loop for j from 0 for with-var in with-vars collect
                  `(,with-var (nth ,j ,with-var-symbol))
                  ))))
     (t (error "Internal error."))
     )))

(defmethod loop-clause-binding 
           ((class (eql 'loop-iterator)) (type t) (ckey t) form clause)
  (declare (ignore clause))
  (case type 
    ((:for-symbols-in :for-symbols-on
      :for-symbols-in-by :for-symbols-on-by 
      :for-symbols-= :for-symbols-=-then)
     (mapcar (lambda (var) (list var nil)) (flatten (second form))))
    ((:while :until) nil)
    (otherwise (list (list (second form) nil)))
    ))

(defmethod loop-clause-declaration ((class t) (type t) (ckey t) form clause)
  (declare (ignore form clause))
  nil)

(defmethod loop-clause-declaration 
           ((class (eql 'loop-iterator)) (type t) (ckey t) form clause)
  (declare (ignore clause))
  (case type
    ((:for-symbol-from :for-symbol-from-by
      :for-symbol-from-to :for-symbol-from-to-by
      :for-symbol-from-downto :for-symbol-from-downto-by
      :for-symbol-from-below :for-symbol-from-below-by)
     `((declare (ignorable ,(second form)))))
    (otherwise nil)
    ))

(defun initial-iterator-bindings (loop-clauses)
  (let ((iter-clauses (extract-clauses-of-class loop-clauses 'loop-iterator)))
    (remove-if
     (lambda (x) (null (second x)))
     (loop for clause in iter-clauses 
           as class = (loop-parse-class clause)
           as type = (loop-parse-type clause)
           as ckey = (loop-parse-ckey clause)
           as form = (loop-parse-form clause)
           collect 
           (list clause (loop-iterator-binding class type ckey form clause))))))

(defmethod loop-iterator-binding ((class t) (type t) (ckey t) form clause)
  (declare (ignore form clause))
  nil)

(defun maybe-create-bblstring-iterator (obj)
  (if (stringp obj) (create-bblstring-iterator obj) obj))

(defmethod loop-iterator-binding 
           ((class (eql 'loop-iterator)) (type t) (ckey (eql :for)) form clause)
  (let ((iter-var (second form))
        (key (loop-parse-key clause)))
    (when (listp iter-var) (setq iter-var (first (flatten iter-var))))
    (block exit
      (list 
       (gensym (formatn "ITER-~A-~A-" key iter-var))
       (ecase type 
         ((:for-symbol-= :for-symbol-=-then :for-symbols-= :for-symbols-=-then)
          (return-from exit nil))
         ((:for-symbol-in :for-symbols-in)
          (let ((obj (fourth form))) 
            (if (and (find-package :bbi)
                     (symbol-value-in-package :*in-bbl-form-processor* :bbi))
                `(iter-init (maybe-create-bblstring-iterator ,obj))
              `(iter-init ,obj))))
         ((:for-symbol-on :for-symbols-on)
          (let ((obj (fourth form))) 
            `(iter-init
              (create-list-on-iterator 
               (verify-iterator-object-is-list ,obj "the ON keyword was used"
               )))))
         ((:for-symbol-in-by :for-symbols-in-by 
           :for-symbol-on-by :for-symbols-on-by)
          (let ((obj (fourth form)) (step (sixth form)))
            `(iter-init 
              (create-general-list-iterator 
               (verify-iterator-object-is-list ,obj "the BY keyword was used") 
               ,(if (symbolp step) `',step step) 
               ,(not 
                 (null
                  (member type '(:for-symbol-on-by :for-symbols-on-by))))))))
         (:for-symbol-from 
          `(iter-init (create-step-iterator ,(fourth form) 1)))
         (:for-symbol-from-by
          `(iter-init (create-step-iterator ,(fourth form) ,(sixth form))))
         (:for-symbol-from-to 
          (iter-range-code (fourth form) (sixth form) :to nil))
         (:for-symbol-from-to-by 
          (iter-range-code (fourth form) (sixth form) :to (eighth form)))
         (:for-symbol-from-downto 
          (iter-range-code (fourth form) (sixth form) :downto nil))
         (:for-symbol-from-downto-by 
          (iter-range-code (fourth form) (sixth form) :downto (eighth form)))
         (:for-symbol-from-below
          (iter-range-code (fourth form) (sixth form) :below nil))
         (:for-symbol-from-below-by
          (iter-range-code (fourth form) (sixth form) :below (eighth form)))
         )))))

(defun iter-range-code (start-form end-form verb by-form)
  (let* ((sc? (realp start-form)) 
        (ec? (realp end-form))
        (bc? (or (null by-form) (realp by-form)))
        (inclusive? (not (eq verb :below)))
        (direction (ecase verb ((:to :below) :up) (:downto :down)))
        (bf (or by-form 1))
        )
    ;; Start and end are numbers and either the BY form is a number or it
    ;; was not provided.
    ;; The error conditions here should be checked in the analysis phase.
    (if (and sc? ec? bc?)
        `(iter-init 
          (create-range-iterator 
           ,start-form ,end-form ,bf ,inclusive? ,direction
           ))
      ;; Something isn't constant.  Use gensyms to store the form values, etc.
      (let ((sfs (gensym "START-"))
            (efs (gensym "END-"))
            (bys (gensym "BY-")))
        `(let ((,sfs ,start-form) (,efs ,end-form) (,bys ,bf))
           ,@(when *safety*
               `((progn
                   ,@(unless sc?
                       `((unless (realp ,sfs)
                           (oops-loop-runtime-iteration-value ,sfs :start))))
                   ,@(unless ec?
                       `((unless (realp ,efs)
                           (oops-loop-runtime-iteration-value ,efs :end))))
                   ,@(unless bc?
                       `((unless (realp ,bys)
                           (oops-loop-runtime-iteration-value ,bys :by))))
                   )))
           ,(case verb
              (:to 
               `(progn 
                  (when (<= ,sfs ,efs)
                    (unless (plusp ,bys)
                      (oops-loop-runtime-iteration-values
                       ,sfs ,efs ,bys :to)))
                  (iter-init 
                   (create-range-iterator ,sfs ,efs ,bys ,inclusive? :up))))
              (:downto
               `(progn 
                  (when (> ,sfs ,efs)
                    (unless (plusp ,bys)
                      (oops-loop-runtime-iteration-values
                       ,sfs ,efs ,bys :downto)))
                  (iter-init 
                   (create-range-iterator ,sfs ,efs ,bys ,inclusive? :down))))
              (:below 
               `(progn 
                  (when (< ,sfs ,efs)
                    (unless (plusp ,bys)
                      (oops-loop-runtime-iteration-values 
                       ,sfs ,efs ,bys :below)))
                  (iter-init 
                   (create-range-iterator ,sfs ,efs ,bys ,inclusive? :up))))
              ))))))

(defun verify-iterator-object-is-list (obj because)
  (when (not (listp obj))
    (oops-loop-must-be-a-list obj because))
  obj)

(defun iterator-steps (loop-clauses)
  (let ((iter-clauses (extract-clauses-of-class loop-clauses 'loop-iterator)))
    (loop for clause in iter-clauses 
          as clause-binding = 
          (second
           (find clause *iter-iterator-clauses-and-bindings* :key 'first))
          append
          (iter-clause-steps clause clause-binding)
          )))

(defun iter-clause-steps (clause clause-binding)
  (let* ((first-iter-var *iter-first-iteration?-var*)
         (form (loop-parse-form clause))
         (type (loop-parse-type clause))
         (iter-var (first clause-binding))
         (loop-var (second form))
         (steps
          (ecase type
            (:while (list (second form)))
            (:until `((not ,(second form))))
            ((:for-symbol-in :for-symbol-on 
              :for-symbol-in-by :for-symbol-on-by
              :for-symbol-from-to :for-symbol-from-to-by
              :for-symbol-from-downto :for-symbol-from-downto-by
              :for-symbol-from-below :for-symbol-from-below-by)
             `((iter-next? ,iter-var) 
               (progn (setq ,loop-var (iter-next ,iter-var)) t)))
            (:for-symbol-= 
             `((progn (setq ,loop-var ,(fourth form)) t)))
            (:for-symbols-=
             (multiple-value-bind (template ignores setqs)
                 (handle-destructuring-template (second form))
               `((destructuring-bind ,template ,(fourth form)
                   ,@(when ignores `((declare (ignore ,@ignores))))
                   ,@setqs
                   t
                   ))))
            (:for-symbol-=-then 
             `((progn 
                 (if ,first-iter-var 
                     (setq ,loop-var ,(fourth form))
                   (setq ,loop-var ,(sixth form)))
                 t)))
            (:for-symbols-=-then 
             (multiple-value-bind (template ignores setqs)
                 (handle-destructuring-template (second form))
               `((destructuring-bind ,template 
                     (if ,first-iter-var ,(fourth form) ,(sixth form))
                   ,@(when ignores `((declare (ignore ,@ignores))))
                   ,@setqs
                   t))))
            ((:for-symbol-from :for-symbol-from-by)
             `((setq ,loop-var (iter-next ,iter-var))))
            ((:for-symbols-in :for-symbols-on 
              :for-symbols-in-by :for-symbols-on-by)
             (multiple-value-bind (template ignores setqs)
                 (handle-destructuring-template (second form))
               `((iter-next? ,iter-var)
                 (destructuring-bind ,template (iter-next ,iter-var)
                   ,@(when ignores `((declare (ignore ,@ignores))))
                   ,@setqs
                   t
                   )))))))
    (if (null *safety*)
        steps
      `((setq *current-loop-key* ,(keywordize (loop-parse-key clause)))
        (setq *current-loop-clause* ,(loop-clause-start-string form))
        ,@steps
        ))))


(defun loop-finally-forms (loop-clauses)
  (let* ((fc (extract-clauses-of-class loop-clauses 'loop-terminator))
         (n (length fc)))
    (cond 
     ((zerop n) nil)
     ((/= n 1) 
      (error "Internal error.  There should only be one FINALLY clause."))
     (t
      (setq fc (first fc))
      (rest (loop-parse-form fc))
      ))))
  

;;; Returns a list of forms to be executed.

(defun loop-execution-and-aggregation-forms (loop-clauses)

  (let ((conditional-clause nil)
        (body-conditional nil)
        (body-clause nil) 
        (agg-conditional nil)
        (aggregation-clause nil)
        (bc-form nil)
        (b-form nil)
        (ac-form nil)
        (a-form nil)
        (body-clause-first? t)
        )
        
    ;; (print 'a)

    ;; find the body and aggregation clauses, if any, and their
    ;; associated conditional clauses (if any)

    (loop for clause in loop-clauses 
          as class = (loop-parse-class clause)
          do
          (case class
            (loop-conditional
             (when conditional-clause
               (error "Internal error. There must be at most 1 cond clause."))
             (setq conditional-clause clause))
            (loop-execution 
             (when body-clause 
               (error "Internal error. There can be at most 1 body clause."))
             (setq body-clause clause)
             (setq body-conditional conditional-clause)
             (setq conditional-clause nil))
            (loop-aggregator
             (when aggregation-clause 
               (error 
                "Internal error.  There can be at most 1 aggregator clause."))
             (when (null body-clause)
               (setq body-clause-first? nil))
             (setq aggregation-clause clause)
             (setq agg-conditional conditional-clause)
             (setq conditional-clause nil))
            (otherwise nil)
            ))

    ;; (print 'b)

    ;; Convert the clauses into forms.

    (when body-conditional
      (setq bc-form (loop-parse-form body-conditional)))
    (when agg-conditional
      (setq ac-form (loop-parse-form agg-conditional)))
    (when body-clause
      (setq b-form
            `(progn
               ;; Distinguish between implied-dos and regular-dos 
               ,@(if (null (loop-parse-key body-clause))
                     (loop-parse-form body-clause)
                   (rest (loop-parse-form body-clause))
                   ))))
    (when aggregation-clause
      (setq a-form (create-aggregation-form aggregation-clause)))

    ;; Add safety if appropriate to forms

    (setq bc-form (add-safety-to-conditional-form bc-form))
    (setq ac-form (add-safety-to-conditional-form ac-form))
    (setq b-form (add-safety-to-do-or-agg-form b-form body-clause))
    (setq a-form (add-safety-to-do-or-agg-form a-form aggregation-clause))

    ;; (print 'c)

    ;; Merge the conditionals with their associated main forms

    (setq b-form
          (cond
           ((and bc-form b-form) (append bc-form (list b-form)))
           (bc-form 
            (error "Internal error. Conditional clause with no body clause!"))
           (b-form b-form)
           (t nil)
           ))
    (setq a-form
          (cond
           ((and ac-form a-form) (append ac-form (list a-form)))
           (ac-form 
            (error "Internal error. Conditional clause with no agg clause!"))
           (a-form a-form)
           (t nil)
           ))

    ;; (print 'd)

    ;; Put the body and aggregation forms together

    (if body-clause-first?
        `(,@(when b-form (list b-form)) ,@(when a-form (list a-form)))
        `(,@(when a-form (list a-form)) ,@(when b-form (list b-form)))
        )
     
    ))


(defun add-safety-to-conditional-form (form)
  (if (and *safety* form)
      `(,(first form)
        (progn
          (setq *current-loop-key* ,(keywordize (first form)))
          (setq *current-loop-clause* ,(loop-clause-start-string form))
          ,(second form)
          ))
    form
    ))

(defun add-safety-to-do-or-agg-form (form clause)
  (if (and *safety* form)
      `(progn
         (setq *current-loop-key* ,(loop-parse-ckey clause))
         (setq *current-loop-clause* 
               ,(loop-clause-start-string (loop-parse-form clause)))
         ,form
         )
    form
    ))


(defun create-aggregation-form (clause)
  (let* ((agg-bindings *iter-aggregation-bindings*)
         (agg-var (first (first agg-bindings)))
         (aux-var (first (second agg-bindings)))
         (form (first (rest (loop-parse-form clause))))
         (type (loop-parse-type clause))
         (expr-var (gensym (formatn "~A-EXPR-" type))))
    `(progn 
       ,(ecase type
          (:count `(when ,form (incf ,agg-var)))
          (:sum `(incf ,agg-var ,form))
          ((:max :min) 
           `(let ((,expr-var ,form))
              (if ,*iter-first-agg?-var* 
                  (setq ,agg-var ,expr-var)
                (setq ,agg-var 
                      (,(if (eq type :max) 'max 'min) ,agg-var ,expr-var))
                )))
          ((:append :nconc) 
           `(let ((,expr-var (,(if (eq type :append) 'copy-list 'progn) ,form)))
              (if (null ,aux-var)
                  (progn 
                    (setq ,agg-var ,expr-var)
                    (setq ,aux-var (last ,expr-var)))
                (progn
                  (setf (cdr ,aux-var) ,expr-var)
                  (setq ,aux-var (last ,aux-var))
                  ))))
          (:collect 
           `(let ((,expr-var (list ,form)))
              (if ,*iter-first-agg?-var* 
                  (progn
                    (setq ,agg-var ,expr-var)
                    (setq ,aux-var ,agg-var))
                (progn 
                  (setf (cdr ,aux-var) ,expr-var)
                  (setq ,aux-var (cdr ,aux-var))
                  )))))
       (setq ,*iter-first-agg?-var* nil)
       )))
          

             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun handle-destructuring-template (template)
  #.(one-string-nl
     "Returns the template with gensyms substituted for the variables,"
     "plus a list of assignments from the gensyms back to the variables,"
     "plus a list of ignored gensyms that aren't setq'ed (because NIL"
     "was in the destructuring template.")
  (let ((ignores nil) (setqs nil))
    (values
     (labels ((recurse (template)
                (cond
                 ((null template) 
                  (let ((nil-gensym (gensym "NIL-")))
                    (push nil-gensym ignores)
                    nil-gensym
                    ))
                 ((eq '&rest template) (list '&rest))
                 ((symbolp template) 
                  (let ((gensym (gensym (s+ (string template) "-"))))
                    (push `(setq ,template ,gensym) setqs)
                    gensym
                    ))
                 ((listp template)
                  (multiple-value-bind (length type)
                      (length-circular-or-dotted? template)
                    (declare (ignore length))
                    (cond
                     ((eq type :circular) 
                      (error "Circular list in destructuring template!"))
                     ((eq type :proper) 
                      (mapcar #'recurse template))
                     ((eq type :dotted)
                      (cons (recurse (car template)) (recurse (cdr template)))
                      ))))
                 (t (error 
                     "Unrecognized object ~A in destructuring template!" 
                     template
                     )))))
       (recurse template))
     (nreverse ignores)
     (nreverse setqs)
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-clauses-lisp-loop-kosher? (loop-clauses)
  (every 
   (lambda (clause) 
     (case (loop-parse-ckey clause)
       (:for (for-clause-kosher? clause))
       (otherwise t)
       ))
   loop-clauses))

(defun the-expression? (expr)
  (and (listp expr) 
       (= (length expr) 3)
       (symbol= :the (first expr))))

(defun for-clause-kosher? (clause)
  (let ((form (loop-parse-form clause)))
    (ecase (loop-parse-type clause)
      ((:for-symbol-on :for-symbol-on-by 
        :for-symbols-on :for-symbols-on-by 
        :for-symbol-= :for-symbol-=-then
        :for-symbols-= :for-symbols-=-then
        :for-symbol-from :for-symbol-from-by)
       t)
      ((:for-symbol-in :for-symbol-in-by
        :for-symbols-in :for-symbols-in-by)
       (let* ((target (fourth form)))
         (cond
          ((constantp target) 
           (let ((ctarget (unquote target)))
             (typep ctarget 'sequence)))
          ((the-expression? target)
           (or 
            (member (second target)
                    '(:list :vector :simple-vector :string :simple-string)
                    :test 'symbol=)
            (and (member (second target) 
                         '(:hash-table :hashtable) :test 'symbol=)
                 (hash-form-kosher? form))))
          (t nil)
          )))
      ((:for-symbol-from-to :for-symbol-from-to-by 
        :for-symbol-from-downto :for-symbol-from-downto-by
        :for-symbol-from-below :for-symbol-from-below-by)
       t)
      )))

(defun hash-form-kosher? (form)
  (let ((vars (second form)))
    (and (listp vars) (= 2 (length vars)) (every 'identity vars))))

(defun render-loop-code-as-lisp-loop (clauses)
  `(locally
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (lisp:loop 
      ,@(loop
         for clause in clauses 
         as ckey = (loop-parse-ckey clause)
         as form = (loop-parse-form clause)
         append
         (case ckey 
           (:implied-do `(do ,@form))
           (:init `(with ,@(cdr form)))
           (:for (render-for-loop-code-as-lisp-loop clause form))
           (otherwise form)
           )))))
         
(defun render-for-loop-code-as-lisp-loop (clause form)
  (maybe-put-back-fixnum 
   (flet ((across-for-in (form) 
            (let ((new-form (copy-list form)))
              (setf (third new-form) :across)
              new-form))
          (to-function-if-symbol-or-quoted-symbol (x)
            (cond 
             ((symbolp x) (list 'function x))
             ((and (listp x) (eq 'quote (first x)))
              (cons 'function (rest x)))
             (t x))))
     (case (loop-parse-type clause)
       ((:for-symbol-in :for-symbols-in :for-symbol-in-by :for-symbols-in-by)
        (let* ((target (fourth form))
               (result 
                (cond
                 ((constantp target) 
                  (let ((ctarget (unquote target)))
                    (if (vectorp ctarget) 
                        (across-for-in form)
                      form)))
                 ((the-expression? target)
                  (ecase (keywordize (second target)) 
                    ((:vector :simple-vector :string :simple-string)
                     (across-for-in form))
                    (:list form)
                    ((:hash-table :hashtable) 
                     (lisp-loop-code-for-hash-loop form))
                    ))
                 (t (error "Internal error...should never get here."))
                 )))
          (when (member (loop-parse-type clause)
                        '(:for-symbol-in-by :for-symbols-in-by))
            (setf (sixth result) 
                  (to-function-if-symbol-or-quoted-symbol (sixth result)))
            )
          result
          ))
       ((:for-symbol-on-by :for-symbols-on-by)
        (let ((new-form (copy-list form)))
          (setf (sixth new-form)
                (to-function-if-symbol-or-quoted-symbol (sixth new-form)))
          new-form))
       (otherwise form)
       ))
   clause
   ))

(defun maybe-put-back-fixnum (form clause)
  (if (member :fixnum (loop-parse-declaration clause) :test 'symbol=)
      (maybe-insert-the-fixnum-forms 
       (append (subseq form 0 2) (list 'lisp:fixnum) (subseq form 2))
       clause)
    form))

(defun maybe-insert-the-fixnum-forms (form clause)
  (case (loop-parse-type clause)
    ((:for-symbol-from-to :for-symbol-from-below 
      :for-symbol-=-then :for-symbol-from-downto :for-symbol-from-by)
     (wrap-with-the-fixnum form '(4 6)))
    ((:for-symbol-from :for-symbol-=)
     (wrap-with-the-fixnum form '(4)))
    ((:for-symbol-from-to-by :for-symbol-from-below-by 
      :for-symbol-from-downto-by)
     (wrap-with-the-fixnum form '(4 6 8)))
    ((:for-symbol-on :for-symbol-on-by)
     (oops-within-loop
      (one-string-nl
       "within the iteration clause ~S,"
       "the iteration method is specified as 'on',"
       "but the iteration variable is declared to be a 'fixnum'."
       "This is inconsistent because 'on' always produces a list,"
       "not a number!")
      (loop-clause-identifier form)
      ))
    (otherwise form)
    ))
    
(defun wrap-with-the-fixnum (form positions) 
  (loop for obj in form 
        for pos from 0 
        collect 
        (if (and (member pos positions) (not (constantp obj)))
            (make-it-fixnum obj) 
          obj)))

(defun make-it-fixnum (form) `(the fixnum ,form))
  

(defun lisp-loop-code-for-hash-loop (form)
  (let* ((vars (second form))
         (keyvar (first vars))
         (valuevar (second vars))
         (target (third (fourth form))))
         
    (cond
     ((symbolp vars) (error "Internal error."))
     ((/= (length vars) 2) (error "Internal error."))
     ((and keyvar valuevar) 
      `(for ,keyvar 
             being the hash-keys of ,target using (hash-value ,valuevar)))
     (keyvar `(for ,keyvar being the hash-keys of ,target))
     (valuevar `(for ,valuevar being the hash-values of ,target))
     (t (error "Internal error.")))))

      
        

     
                     
                     
       
     
            
       
    
