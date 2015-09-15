;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE bbi)

;;; Authors: JP Massar, Jeff Elhai, Mark Slupesky

;;; The INCREMENT and DECREMENT toplevel, exported macros.

;;; Note:  This treats a NIL value as if it were 0 !!


(DEFINE-MACRO Increment
  SUMMARY "Increments entity by 1 or by a specified value"
  REQUIRED entity
  KEYWORD by = 1
  BODY
  `(increment-or-decrement ,entity ,by t))

(DEFINE-MACRO Decrement
  SUMMARY "Decrements entity by 1 or by a specified value"
  REQUIRED entity
  KEYWORD by = 1
  BODY
  `(increment-or-decrement ,entity ,by nil))


;;; Make sure increment value is a number; negate it if necessary
;;; and call the workhorse macro, BBL-INCREMENT

(defmacro increment-or-decrement (entity by positive?)
  (let ((f (if positive? 'increment 'decrement)))
    (IF *safety*
        (if (numberp by) 
            `(BBL-INCREMENT ,entity ,(if positive? by `(- ,by)) t ,f)
          (let ((by-symbol (gensym "BY-")))
            `(let ((,by-symbol 
                    (verify-number-and-maybe-negate 
                     ,by ,positive? 
                     ,(if positive? "Increment value" "Decrement value"))))
               (bbl-increment ,entity ,by-symbol t ',f)
               )))
      `(bbl-increment ,entity ,(if positive? by `(- ,by)) nil ,f)
      )))


(DEFMACRO BBL-INCREMENT (entity by safely? f)

  (LET* ((reference? (AND (LISTP entity) (EQ (FIRST entity) 'UTILS:REF)))
         (literal-list? (AND (LISTP entity) (NOT reference?)))
         (defined-locally? (and (symbolp entity) (DEFINED-LOCALLY entity)))
         )

    (FLET ((Increment-it () 
             (funcall 
              (if safely? 'increment-it-safely 'increment-it-unsafely)
              entity by
              )))

      ;; (increment x 6)

      (COND
       ((SYMBOLP entity)
        (IF defined-locally?
            (INCREMENT-IT)
          `(IF (DEFINED-GLOBALLY ',entity) ,(INCREMENT-IT) (SETQ ,entity ,by))
          ))
   
       ;; (increment {a b c} 5)
       ;; (increment (a b c) 5)
       ;; (increment (x[y] z[q]) 4)

       ;; This allows (increment (a b (c d)) 3) because it just recurses

       (literal-list?
        (LET ((place-list 
               (IF (EQ (FIRST entity) 'bbi::%CURLY-LIST%) 
                   (REST entity)
                 entity)))
          `(progn 
             ,@(LOOP FOR place IN place-list
                    COLLECT `(BBL-INCREMENT ,place ,by ,safely? ',f)))))
       
       ;; (increment x[4] 3)
       ;; doesn't work for (increment x[y[4]] 1)

       (reference?
        (generate-reference-increment-code entity by t)
        )

       ;; (increment 3)

       ((NUMBERP entity) 
        (ERROR "~A works on variables, not on numbers like ~A" f entity))

       ;; We fail here if the user types something like
       ;; (increment (first x) 5), along with random stuff.
                                      
       (T (ERROR "Don't know how to ~A ~S" f entity))

       ))))


(defun generate-reference-increment-code (entity by safely?)
  (flet ((increment-it () 
           (funcall 
            (if safely? 'increment-it-safely 'increment-it-unsafely)
            entity by)))
    (LET ((referent (SECOND entity))
          (indices (CDDR entity)))
      (COND
       ((NULL indices)
        (ERROR (ONE-STRING "Global INCREMENT of tables not yet supported."
                           "~&Table '~S' must have indices.")
               referent))
       ((DEFINED-LOCALLY referent) (INCREMENT-IT))
       ((NOT (SYMBOLP referent)) (INCREMENT-IT))
       (T 
        ;; Make sure the indices aren't evaluated twice.
        ;; This should be made to only generate gensyms for
        ;; expressions, not constants or symbols.  This is probably
        ;; going to be a generally useful piece of code.
        (let ((indices-symbols
               (loop for j from 0 below (length indices)
                     collect (gensym "INDEX-"))))
          (setq entity `(utils:ref ,referent ,@indices-symbols))
          `(let ,(loop for s in indices-symbols
                       for i in indices 
                       collect (list s i))
             (UNLESS (DEFINED-GLOBALLY ',referent)
               (SETF ,referent 
                     (new-increment-table (list ,@indices-symbols))
                     ))
             ,(INCREMENT-IT)
             )))))))

(defun increment-it-safely (entity by)
  `(setf ,entity (increment-value+ ,entity ,by)))

(defun increment-it-unsafely (entity by) 
  `(setf ,entity (incf ,entity ,by)))

(defun verify-number-and-maybe-negate (inc-value positive? verify-string)
  (setq inc-value (verify-number inc-value verify-string))
  (when (not positive?) (setq inc-value (- inc-value)))
  inc-value)

;; Creates a new garray when INCREMENT tries to access a non-existent
;; object.  The new garray is created such that any reference to any 
;; index that doesn't exist will return 0 instead of erroring out.  
;; This allows one to increment any element of a garray created by 
;; the increment macro, but not necessarily any other garray if the index
;; is not valid.  

(defun new-increment-table (indices)
  (utils:make-garray
   (parse-table-specs (string-to-$ indices))
   :adjustable t 
   :initial-element 0
   :if-accessed-location-does-not-exist 0
   ))

(DEFGENERIC Increment-Value+ (x y)
  (:documentation
   #.(one-string-nl
      "If X has numeric value, increment it by Y and return the new value."
      "If X is a list, increment each element by Y in place, returning the"
      "  modified list X."
      "If X is NIL, return Y."
      "If X is anything else, error out."
      "If X is a list and any element of X is not a number, error out."
      "Y is defined to be numeric; no checking is done for Y."
      )))

(DEFMETHOD Increment-Value+ ((x T) y)
  (declare (ignore y))
  (ERROR "Don't know how to increment/decrement ~S, an object of type ~S" 
         x (TYPE-OF x)))

(DEFMETHOD Increment-Value+ ((x Number) y) (+ x y))

(DEFMETHOD Increment-Value+ ((x List) y)
  (LOOP FOR rest-of-list ON x 
        AS elem = (FIRST rest-of-list)
        DO
        (VERIFY-NUMBER elem "Increment/decrement element in list")
        (SETF (FIRST rest-of-list) (+ elem y))
        FINALLY (RETURN x)
        ))

(DEFMETHOD Increment-Value+ ((x Null) y) y)




#||

(defmacro updatef (&environment env place function &rest values)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    `(let ,(mapcar (function list) vars vals)
       (let ,(mapcar (function list) 
                      store-vars
                      `((,function ,reader-form ,@values)))
         ,writer-form))))

||#
