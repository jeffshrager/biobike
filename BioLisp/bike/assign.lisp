;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE bbi)

;;; Authors: JP Massar, Jeff Elhai, Mark Slupesky

; ======================= NEW ASSIGN ===========================

;;; Parses (ASSIGN T1 [AS | =] V1 T2 [AS | =] V2 ... [DISPLAY-OFF])
;;; into constitutent assignments, then calls ASSIGN-ONE-VALUE
;;; to do code generation for each assignment.

(defmacro assign

          (&rest 
           target-value-goo 
           &aux 
           (display-off nil)
           target-value-pairs
           target
           (state :target))

  ;; Allows DISPLAY-OFF flag as very last token.

  (when (symbol= (lastelem target-value-goo) :display-off)
    (setq target-value-goo (butlast target-value-goo))
    (setq display-off t))
    
  (loop for elem in target-value-goo
        for count from 2
        do
        (flet ((as? (x) (or (symbol= x :as) (symbol= x :=) (SYMBOL= x :<-)))
               (oops-as (what)
                 (error
                  (one-string-nl
                   "Illegal ASSIGN syntax in the ASSIGN statement"
                   "~S."
                   "Problem: 'AS', '=', or '<-' appears where the ~A of the"
                   "assignment clause should be."
                   "Error occurs in the ~:R position of the ASSIGN statement.")
                  (cons 'assign target-value-goo)
                  what
                  count
                  )))
          (ecase state
            (:target
             (when (as? elem) (oops-as "target"))
             (setq target elem)
             (setq state :as)
             )
            (:as
             (if (as? elem)
                 (setq state :value)
               (progn
                 (push (list target elem) target-value-pairs)
                 (setq state :target)
                 )))
            (:value
             (when (as? elem) (oops-as "value expression"))
             (push (list target elem) target-value-pairs)
             (setq state :target)
             ))))

  (unless (eq state :target)
    (error
     (one-string-nl
      "Illegal ASSIGN syntax in the ASSIGN statement"
      "~S."
      "Problem: The last target, ~S, has no corresponding value expression."
      "ASSIGN takes one or more clauses, each of which consists of"
      "a target, followed optionally by 'as' or '=', followed by a value.")
     (cons 'assign target-value-goo)
     (lastelem target-value-goo)
     ))

  `(let ((*bbl-level* (if (zerop *bbl-level*) 2 
                        (the fixnum (1+ *bbl-level*))))) 
     ,(if (= (length target-value-pairs) 1)
          (assign-one-target (first target-value-pairs) display-off)
        `(PROGRAM 
          ,@(mapcar 
             (lambda (x) (assign-one-target x display-off))
             (reverse target-value-pairs)
             )))
     ))


(defun assign-one-target (target-and-value display-off)

  (flet ((possible-return (target value)
           (if (not display-off)
               `(setf ,target ,value)
             `(possibly-return nil (setf ,target ,value))
             )))

    (destructuring-bind (target value) target-and-value

      (cond

       ((symbolp target)
        (when (constantp target)
          (error 
           (one-string-nl
            "Illegal ASSIGNMENT in the ASSIGN statement"
            "~S."
            "The symbol ~S is a constant!"
            "You cannot assign a new value to a constant.")
           (list 'assign target value)
           target
           ))
        (possible-return target value))

       ;; (assign x 5) or (assign x[2] 5)
       ((and (listp target) (symbol= :ref (first target)))
        (possible-return target value))

       ;; (assign 3 5)
       ((not (listp target))
        (error "Illegal ASSIGN syntax: The target ~S is not valid." target))

       ;; Degenerate case.  (assign (x) 5) --> (assign x 5)
       ((= (length target) 1) 
        (assign-one-target (list (first target) value) display-off))

       ;; Handle curly-bracket syntax: (assign {x y} 3)
       ((eq (first target) 'bbi::%curly-list%)
        (assign-one-target (list (cdr target) value) display-off))

       (t

        (cond

         ;; (assign (x y z) 5) or (assign (x[2] y[z]) 5)
         ((and (constantp value) (not (listp value)))
          ;; symbols can evaluate to lists, so we can't simplify them.  
          ;; ((or (symbolp value) 
          ;; (and (constantp value) (not (listp value))))
          `(progn
             ,@(loop for targets on target
                     as next-target = (first targets)
                     collect
                     (if (null (cdr targets))
                         (possible-return next-target value)
                       `(setf ,next-target ,value)
                       ))))

         ;; (assign (x y z) (f 5)), or (assign (x[2] y[z]) (f 5))

         (t

          (let ((results (gensym "RESULTS-"))
                (single-result? (gensym "SINGLE-RESULT?-"))
                (result-to-use (gensym "RESULT-TO-USE-"))
                (scalar-single-result? (gensym "SCALAR-SINGLE-RESULT-"))
                )

            ;; Generate code to determine whether a single result
            ;; or multiple values were returned.  If a single result
            ;; was returned, store that single result into each target,
            ;; otherwise store each multiple result (or NIL, if there
            ;; aren't enough results) into target, respectively.

            `(let* ((,results (multiple-value-list ,value))
                    (,single-result? (= (length ,results) 1))
                    (,result-to-use 
                     (if ,single-result? (first ,results) ,results))
                    (,scalar-single-result? (atom ,result-to-use))
                    )

               ,@(loop 
                  for targets on target
                  as next-target = (first targets)
                  collect
                  (if (null (cdr targets))
                      (possible-return 
                       next-target
                       `(if ,scalar-single-result? 
                            ,result-to-use (pop ,result-to-use)))
                    `(setf 
                      ,next-target
                      (if ,scalar-single-result? 
                          ,result-to-use (pop ,result-to-use)))
                    )))))

         ))))))

           
#+test
(define-macro fred required x body `(list ,x))

(DEFINE-MACRO Define
  SUMMARY "Initializes one variable to a given value"
  REQUIRED (target (AS = <-) assignment)
  FLAG Display-off
  FLAG Labeled
  ;; INITIALIZE ref = 'REF
  PUBLIC
  BODY
  `(let ((*bbl-level* (if (zerop *bbl-level*) 2 
                        (the fixnum (1+ *bbl-level*))))) 
     ,(flet 
          ((setf-or-possible-return ()
             ;; Generally, displaying results will be enabled, so just
             ;; expand to setf, which returns value of assignment.
             (COND 
              (labeled
               `(COND
                 ((NOT (STRINGP ,assignment))
                  (ERROR 
                   (S+ "Can't use LABELED flag in a definition unless "
                       "what's being assigned is a string, and this isn't: ~A")
                   ,assignment))
                 (,display-off
                  (POSSIBLY-RETURN 
                   NIL
                   (SETF 
                    ,target 
                    (MAKE-LABELED-SEQUENCE :SEQUENCE ,assignment
                                           :LABEL (SYMBOL-NAME ',target)))))
                 (T (SETF ,target (MAKE-LABELED-SEQUENCE 
                                   :SEQUENCE ,assignment
                                   :LABEL (SYMBOL-NAME ',target))))))
              (display-off
               `(possibly-return nil (setf ,target ,assignment)))             
              (T `(setf ,target ,assignment)))))

        (cond
         ;; x <- y
         ((not (listp target))
          (define-symbol-code target assignment labeled display-off)
          )
         ;; not x[i] <- y 
         ((not (equal (first target) 'ref))           
          `(assign ,target ,assignment ,@(when display-off `(display-off))))
         ;; x[i] <- y 
         (t                                     
          (let ((table (second target)) (indices (cddr target)))
            (cond
             ((defined-locally table) (setf-or-possible-return))
             (t
              `(if (defined-globally ',table)
                   ,(setf-or-possible-return)
                 ,(define-new-table table indices assignment display-off)
                 )))))))))

#+obsolete
(defun define-symbol-code (sym value labeled? display-off?)
  (let ((v (gensym "VALUE-")))
    `(progn
       (check-if-external-symbol-already-initialized ',sym)
       (defparameter ,sym 
         (prog1
             (let ((,v ,value))
               ,@(when labeled? 
                   `((unless (stringp ,v)
                       (error
                        (one-string-nl
                         "Can't use LABELED flag in a definition unless what's"
                         "being assigned is a string, and this isn't: ~A.")
                        ,v
                        ))
                     (setq 
                      ,v 
                      (make-labeled-sequence :sequence ,v :label ,(string sym))
                      )))
               ,v
               )
           (mark-external-symbol-as-initialized ',sym)
           ))
       ,@(when display-off? `((possibly-return nil ,sym)))
       ,@(unless display-off? `(,sym))
       )))

(defun define-symbol-code (sym value labeled? display-off?)
  (let ((v (gensym "VALUE-")))
    `(progn
       (check-if-external-symbol-already-initialized ',sym)
       (defparameter ,sym 
         (prog1
             (let ((,v ,value))
               ,@(when labeled? 
                   `(without-code-walker
                      (error-if-labeled-for-define-not-ok? ,v)
                      (etypecase ,v
                        (string ; ((or string list)
                         (setq ,v 
                               (make-labeled-sequence
                                :sequence ,v :label ,(string sym))))
                        (table 
                         (assign-label-to-table ,v ,(string sym))
                         ))))
               ,v
               )
           (mark-external-symbol-as-initialized ',sym)
           ))
       ,@(when display-off? `((possibly-return nil ,sym)))
       ,@(unless display-off? `(,sym))
       )))

(defun error-if-labeled-for-define-not-ok? (v)
  (typecase v
    ((or string table list) t)
    (t 
     (error 
      (one-string-nl
       "Cannot use LABELED flag in a definition unless value being assigned"
       "is a string or a table.  (~A is a ~A.)"
       ;; "is a string, table, or a list (~A is a ~A.)"
       )
      v (type-of v)
      ))))

(defun assign-label-to-table (table label)
  (setf (utils::garray-named table) label))
      

(defun check-if-external-symbol-already-initialized (sym)
  (let ((uname wb::*username*))
    (when uname 
      (when (not (eq (symbol-package sym) (find-package uname)))
        (when (boundp sym)
          (let ((assigner (get sym :assigner)))
            (when (and assigner (not (eq uname assigner)))
              (error 
               (one-string-nl
                "Sorry!  The variable name '~A' has already been taken, either"
                "by another user or the system.  Were you to use this name and"
                "give '~A' a value, someone else might change the value"
                "of '~A' without your knowing it."
                ""
                "To avoid this happening, you are not allowed to define it."
                "Please use a different name for your variable.  Thanks!"
                )
               sym sym sym
               ))))))))

;; There's a race condition here...
(defun mark-external-symbol-as-initialized (sym)
  (let ((uname wb::*username*))
    (when uname
      (when (not (eq (symbol-package sym) (find-package uname)))
        (setf (get sym :assigner) uname)
        ))))

(defun define-new-table (table indices assignment display-off)
  (flet ((generate-code (indices)
           `(progn
              (setf ,table (NEW-TABLE (string-to-$ (list ,@indices))
                                         ,@(when display-off `('display-off))))
              ,(if (not display-off)
                   `(setf (gref ,table ,@indices) ,assignment)
                 `(possibly-return 
                   nil (setf (gref ,table ,@indices) ,assignment)
                   )))))
    (if (every (lambda (x) (or (symbolp x) (constantp x))) indices)
        (generate-code indices)
      (let ((isymbols (mapcar
                       (lambda (x) (declare (ignore x)) (gensym "I-SYMBOL-")) 
                       indices)))
        `(let (,@(mapcar 'list isymbols indices))
           ,(generate-code isymbols)
           ))
      )))

