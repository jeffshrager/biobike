;;; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :aframes)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

(defun defslot 
       (frame 
        &rest args
        &key
        (base #$sys.slot)
        (predicate nil)
        (set-valued? nil)
        (list-valued? nil)
        &allow-other-keys
        )
  #.(one-string-nl
     "Defines frame to have various properties:"
     "  BASE : if BASE is non-NIL (default #$slot), BASE is added to FRAME's"
     "#$isA list if not already there."
     "  VALUE-TYPE :  If non-NIL, FRAME is given a #$ValueType slot with"
     "    this value."
     "FRAME is returned, suitably modified.")
  (declare (ignore args))
  (when base (pushnew base (slotv frame #$sys.isa)))
  (when predicate 
    (enable-frame-flag frame :slot-domain)
    (setf (slotv frame #$sys.DomainPredicate) predicate))
  (when (and list-valued? set-valued?)
    (error "Slot data cannot be both list and set valued."))
  (when set-valued? 
    (enable-frame-flag frame :slot-domain)
    (enable-frame-flag frame :slot-domain-set?))
  (when list-valued? 
    (enable-frame-flag frame :slot-domain)
    (enable-frame-flag frame :slot-domain-list?))    
  frame)

(defun derived-slot-function-name (slot pre post)
  (intern (string-upcase (format nil "~A-~A-~A" pre (fname slot) post))))

;;; Computed slots

(defmacro def-computed-slot 
          ((slot &optional (frame-symbol 'frame) (slot-symbol 'slot))
           &body body
           )
  #.(one-string-nl
     "Define a slot whose value is only computed when that value is"
     "accessed for the first time.  BODY defines code that gets executed"
     "to compute said value within the current lexical environment plus"
     "bindings of the values of FRAME-SYMBOL and SLOT-SYMBOL to the frame"
     "and slot being accessed.")
  `(create-computed-slot 
    ,slot
    ;; Define the accessor function, which tests to see if the slot
    ;; has a non-nil value, and if not, sets the value to the result
    ;; of evaluating BODY (in the current lexical environment).
    (lambda (,frame-symbol ,slot-symbol)
      (setf (%slotv ,frame-symbol ,slot-symbol)
            (progn ,@body)
            ))))

(defun create-computed-slot (slot f)
  (let ((computed-fname 
         (derived-slot-function-name slot "COMPUTED" "ACCESSOR"))
        (cf (if (compiled-function-p f) f (compile nil f))))
    (setf (symbol-function computed-fname) cf)
    (make-computed-slot slot computed-fname)
    (pushnew #$sys.ComputedSlot (slotv slot #$sys.isa))
    computed-fname
    ))

(defmacro def-always-computed-slot 
          ((slot &optional (frame-symbol 'frame) (slot-symbol 'slot))
           &body body
           )
  #.(one-string-nl
     "Define a slot whose value is always computed instead of being stored."
     "BODY defines code that gets executed to compute the value within the"
     "current lexical environment plus bindings of the values of FRAME-SYMBOL"
     "and SLOT-SYMBOL to the frame and slot being accessed.")
  `(create-always-computed-slot 
    ,slot
    ;; Define the accessor function, which tests to see if the slot
    ;; has a non-nil value, and if not, sets the value to the result
    ;; of evaluating BODY (in the current lexical environment).
    (lambda (,frame-symbol ,slot-symbol) 
      (declare (ignorable ,frame-symbol ,slot-symbol))
      (progn ,@body)
      )))

(defun create-always-computed-slot (slot f)
  (let ((computed-fname 
         (derived-slot-function-name slot "COMPUTED" "ACCESSOR"))
        (cf (if (compiled-function-p f) f (compile nil f))))
    (setf (symbol-function computed-fname) cf)
    (make-always-computed-slot slot computed-fname)
    (pushnew #$sys.AlwaysComputedSlot (slotv slot #$sys.isa))
    computed-fname
    ))

(defun make-computed-slot (slot function)
  #.(one-string-nl
     "Dynamically define a computed slot.  FUNCTION should denote a function"
     "of two arguments, the FRAME being accessed and SLOT.  If FUNCTION is an"
     "actual function object instead of a symbol then the SLOT frame will not"
     "be file dumpable.")
  (when (functionp function)
    (error "Cannot use actual function for COMPUTED-SLOT. Use a symbol."))
  (enable-frame-flag slot :slot-computed)
  (setf (slotv slot #$sys.getter) function))

(defun make-always-computed-slot (slot function)
  #.(one-string-nl
     "Dynamically define a computed slot.  FUNCTION should denote a function"
     "of two arguments, the FRAME being accessed and SLOT.  If FUNCTION is an"
     "actual function object instead of a symbol then the SLOT frame will not"
     "be file dumpable.")
  (when (functionp function)
    (error "Cannot use actual function for COMPUTED-SLOT. Use a symbol."))
  (enable-frame-flag slot :slot-always-computed)
  (setf (slotv slot #$sys.getter) function))


;;; Inverses

(defun add-put-demon (slot demon) 
  (when (functionp demon) 
    (error "Cannot use actual function as PUT DEMON.  Use a symbol."))
  (pushnew demon (slotv slot #$sys.putDemons))
  (enable-frame-flag slot :slot-change)
  demon)

;;; Now, let's use the put-demons to maintain inverse slots

;;; Note this just handles new values -- 
;;; doesn't do anything for disappearing values! 

(defun slot-must-contain-only-frames (frame slot inverse-slot bad-element)
  (error
      (one-string
       "You are trying to have the ~A slot of frame ~A contain ~A. "
       "But that value is not a frame, and since the ~A frame "
       "is defined as having an inverse (~A), every element of "
       "the value of ~A's slot ~A must be a frame.")
    slot frame bad-element slot inverse-slot frame slot
    ))

(defun verify-inverse-slot (inverse-slot slot)
  (unless inverse-slot 
    (error "Internal error. Slot ~A has no #$sys.Inverse value!" slot))
  (unless (framep inverse-slot)
    (error 
     (one-string
      "Ruh Roh. Internal error. #$sys.Inverse value, ~A, for slot ~A, "
      "is not a slot!") 
     inverse-slot slot
     )))

;;; Doing a PUSHNEW is much more expensive than just doing a PUSH.
;;; So we only do the PUSHNEW, risking duplicate entries if not, when
;;; specifically instructed to via the DEF-INVERSE-SLOT declaration.

(defun maybe-adjust-inverse-slot 
       (frame new-value inverse-slot prevent-duplicates?)
  (if prevent-duplicates?
      (pushnew frame (%slotv new-value inverse-slot) :test #'eq)
    (push frame (%slotv new-value inverse-slot))
    ))

(defun generic-inverse-slot-put-daemon 
       (frame slot new-list prevent-duplicates?)
  (unless (listp new-list)
    (error 
     (one-string 
      "You are trying to have the ~A slot of frame ~A have value ~A. "
      "But that slot must contain a list, not a singleton.")
     slot frame new-list))
  (let ((old-values (slotv frame slot))
        (inverse-slot (slotv slot #$sys.Inverse)))
    (verify-inverse-slot inverse-slot slot)
    (cond
     ;; We've been triggered via PUSHNEW and the value being pushed is in
     ;; fact not new.  So the inverse relationship that get's established here
     ;; should already have been done, so there is nothing to do.
     ((eq new-list old-values) nil)
     ;; We've been triggered via PUSHNEW and the element is new.
     ;; The new element has been CONS'ed on to the old list, so it is
     ;; the first element of NEW-LIST while the rest of NEW-LIST is identical
     ;; (EQ) to the old list.
     ((eq (cdr new-list) old-values)
      (let ((v (first new-list)))
        (unless (framep (first new-list))
          (slot-must-contain-only-frames frame slot inverse-slot v))
        (maybe-adjust-inverse-slot frame v inverse-slot prevent-duplicates?)
        ))
     ;; We're doing a standard SETF of the slot's value,
     ;; replacing the old value entirely.  See if any of the new values
     ;; were old values; if so, we don't need to do anything for that value.
     ;; Otherwise establish the inverse relationship as appropriate.
     ;; Note that we don't ever 'undo' the inverse relationship, even if
     ;; we effectively remove old values by setting the slot to have 
     ;; different values.  (Essentially, once an #$isA, always a #$isA.)
     (t
      (dolist (v new-list)
        (unless (framep v) 
          (slot-must-contain-only-frames frame slot inverse-slot v))
        (unless (member v old-values :test 'eq)
          (maybe-adjust-inverse-slot frame v inverse-slot prevent-duplicates?)
          ))))))

(defun remove-inverted-element (frame slot element-frame)
  (let ((inverse-slot (slotv slot #$sys.inverse))
        (old-list (slotv frame slot)))
    (unless (listp old-list)
      (error "The value of slot ~A of frame ~A is not a list..." frame slot))
    (setf (slotv frame slot) (delete element-frame old-list))
    (when inverse-slot
      (let ((old-inverse-list (slotv element-frame inverse-slot)))
        (setf (slotv element-frame inverse-slot) 
              (delete frame old-inverse-list)
              )))
    frame
    ))
      
#|

  Example using PARTS and PARTOF.

  Suppose we have a #$COMPUTER frame, and we decide that a #$CPU is to have
  the property that it will be #$PARTOF a #$COMPUTER.  
  So what we would do in our code is:

  (pushnew #$COMPUTER (slotv #$CPU #$PARTOF))  

  Now the CPU 'knows' it is part of a computer, but the COMPUTER does not
  automatically know that one of its #$PARTS is a CPU.

  What we want to automagically have happen is for the code

  (pushnew #$CPU (slotv #$COMPUTER #$PARTS))

  to be executed.

  By defining PARTS and PARTOF to be inverses using the statement

  (DEF-INVERSE-SLOT PARTS PARTOF)

  we cause this to happen.  (Note that the inverse does NOT happen!  If we
  were to execute the code we want to happen automatically, the original code
  we used would not get triggered.  See below, DEF-RECIPROCAL-INVERSE-SLOTS
  
|#



(defmacro def-inverse-slot (inverse-slot slot &key (prevent-duplicates? nil))
  `(create-inverse-slot
    ,inverse-slot ,slot
    (lambda (frame slot new-list)
      (generic-inverse-slot-put-daemon
       frame slot new-list ,prevent-duplicates?))
    ))

(defun create-inverse-slot (inverse-slot slot pf)
  (let ((put-function-name 
         (derived-slot-function-name slot "INVERSE" "PUT-DAEMON"))
        (cpf (if (compiled-function-p pf) pf (compile nil pf))))
    (defslot inverse-slot :set-valued? t)
    (defslot slot :set-valued? t)
    (setf (slotv inverse-slot #$sys.Inverse) slot)
    (setf (slotv slot #$sys.Inverse) inverse-slot)
    (setf (symbol-function put-function-name) cpf)
    (add-put-demon slot put-function-name)
    put-function-name
    ))


(defmacro def-reciprocal-inverse-slots 
          (slota slotb &key (prevent-duplicates? nil))
  `(progn 
     (def-inverse-slot ,slota ,slotb :prevent-duplicates? ,prevent-duplicates?)
     (def-inverse-slot ,slotb ,slota :prevent-duplicates? ,prevent-duplicates?)
     ))

(def-reciprocal-inverse-slots
 #$sys.isa #$sys.SubClasses :prevent-duplicates? t)

(def-reciprocal-inverse-slots
 #$sys.PartOf #$sys.Parts :prevent-duplicates? t)

(def-reciprocal-inverse-slots
 #$sys.IsAnInstanceOf #$sys.InstancesOf :prevent-duplicates? nil)

;;; TRANSITIVE CLOSURE SLOTS

;;; These transitive closure slots are not maintained under db changes

(defun compute-transitive-slot (frame slot)
  (compute-transitive-closure frame (slot-accessor slot)))

(defun compute-transitive-closure (frame generator)
  (let ((result nil))
    (recursive-descent 
     frame generator (lambda (child) (pushnew child result)))
    result))

;;; A generalized depth-first tree walker
(defvar *recursive-descent-level* 0)
(defun recursive-descent (start child-generator procedure)
  (funcall procedure start)
  (let ((*recursive-descent-level* (+ *recursive-descent-level* 1)))
    (dolist (child (funcall child-generator start))
      (recursive-descent child child-generator procedure))))

(defmacro def-transitive-slot (tslot oslot)
  `(progn
     (defslot ,tslot :set-valued? t)
     (def-computed-slot (,tslot frame slot)
       (compute-transitive-slot frame ,oslot)
       )))

(defun recursive-print 
       (start child-generator &optional (print-function #'princ))
  (recursive-descent 
   start child-generator
   (lambda (node)
     (terpri)
     (dotimes (n *recursive-descent-level*) (princ "  "))
     (funcall print-function node))))

;;; Inheriting slots

(defun def-inherited-slot (slot &optional (inherits-through #$sys.isA))
  #.(one-string-nl
     "define a SLOT whose value, if it does not exist, is computed from "
     "the SLOT value of one of the frames it inherits through, transitively "
     "(inheritance defaulting to an ISA relationship).  If more than "
     "one frame is a direct parent of SLOT via the inheritance relationship "
     "the search for a non-nil value proceeds depth-first not breadth-first. ")
  (defslot slot)
  (setf (slotv slot #$sys.getter) 'inheriting-get)
  (enable-frame-flag slot :slot-inherited)
  (setf (slotv slot #$sys.InheritsThrough) inherits-through))

(defun inheriting-get (frame slot)
  ;; does depth-first search of parents, not the right thing.
  (block exit
    (let ((parents (slotv frame (slotv slot #$sys.InheritsThrough))))
      (dolist (parent parents nil)
        (multiple-value-bind (rval found?)
            (slotv parent slot)
          (when found? (return-from exit (values rval found?))))))
    (values nil nil)
    ))




