;;; -*- Package: frames; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :frames)

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
        &key
        (base #$slot)
        (applicable-to nil) ;default is applies to everything
        (value-type nil) ;default is anything
        (set-valued? nil)
        )
  #.(one-string-nl
     "Defines frame to have various properties:"
     "  BASE : if BASE is non-NIL (default #$slot), BASE is added to FRAME's"
     "#$isA list if not already there."
     "  APPLICABLE-TO :  If non-NIL, FRAME is given an #$ApplicableTo"
     "    slot with this value."
     "  VALUE-TYPE :  If non-NIL, FRAME is given a #$ValueType slot with"
     "    this value."
     "FRAME is returned, suitably modified.")
  (when base (pushnew base (slotv frame #$isA)))
  (when applicable-to (setf (slotv frame #$applicableTo) applicable-to))
  (when value-type (setf (slotv frame #$valueType) value-type))
  (when set-valued? (setf (slotv frame #$SetValued) t))
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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (create-computed-slot 
      ,slot
      ;; Define the accessor function, which tests to see if the slot
      ;; has a non-nil value, and if not, sets the value to the result
      ;; of evaluating BODY (in the current lexical environment).
      (lambda (,frame-symbol ,slot-symbol)
        (or (%slotv ,frame-symbol ,slot-symbol)
            (setf (%slotv ,frame-symbol ,slot-symbol)
                  (progn ,@body)
                  ))))))

(defun create-computed-slot (slot f)
  (let ((computed-fname 
         (derived-slot-function-name slot "COMPUTED" "ACCESSOR"))
        (cf (if (compiled-function-p f) f (compile nil f))))
    (setf (symbol-function computed-fname) cf)
    (make-computed-slot slot computed-fname)
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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (create-always-computed-slot 
      ,slot
      ;; Define the accessor function, which tests to see if the slot
      ;; has a non-nil value, and if not, sets the value to the result
      ;; of evaluating BODY (in the current lexical environment).
      (lambda (,frame-symbol ,slot-symbol) 
        (declare (ignorable ,frame-symbol ,slot-symbol))
        (progn ,@body)
        ))))

(defun create-always-computed-slot (slot f)
  (let ((computed-fname 
         (derived-slot-function-name slot "COMPUTED" "ACCESSOR"))
        (cf (if (compiled-function-p f) f (compile nil f))))
    (setf (symbol-function computed-fname) cf)
    (make-computed-slot slot computed-fname)
    (pushnew #$AlwaysComputedSlot (slotv slot #$isA))
    computed-fname
    ))

(defun make-computed-slot (slot function)
  #.(one-string-nl
     "Dynamically define a computed slot.  FUNCTION should denote a function"
     "of two arguments, the FRAME being accessed and SLOT.  If FUNCTION is an"
     "actual function object instead of a symbol then the SLOT frame will not"
     "be file dumpable.")
  ;; Make SLOT be an instance of #$ComputedSlot
  (defslot slot :base #$computedSlot)
  (setf (slotv slot #$getter) function))


;;; Inverses

(defparameter *daemon-duplication-warning*
  (one-string
   "*** Adding function instead of symbol to ~A daemon list for slot ~A"
   "*** If you are recompiling/reloading the daemon list will contain"
   "*** duplicate function objects since PUSHNEW is used and a new function"
   "*** object is created each compilation/load.  Thus the daemon will"
   "*** get executed multiple times.  The solution is to use a symbol"
   "*** which is fbound to the function object you wish to use as a daemon."
   ))

(defun add-put-demon (slot demon) 
  (when (functionp demon) (warn *daemon-duplication-warning* "add" slot))
  (pushnew demon (slotv slot #$putDemons)))
(defun add-add-demon (slot demon) 
  (when (functionp demon) (warn *daemon-duplication-warning* "put" slot))
  (pushnew demon (slotv slot #$addDemons)))  

;;; Now, let's use the put-demons to maintain inverse slots

;;; Note this just handles new values -- 
;;; doesn't do anything for disappearing values! 

(defun slot-must-contain-only-frames (frame slot inverse-slot bad-element)
  (error
   (one-string
    "You are trying to have the ~A slot of frame ~A contain ~A. "
    "But that value is not a frame, and since the ~A frame "
    "is defined as having an inverse (~A), every element of "
    "the value of ~A's slot ~A must be a frame."
    slot frame bad-element slot inverse-slot frame slot
    )))

(defun verify-inverse-slot (inverse-slot slot)
  (unless inverse-slot 
    (error "Ruh Roh. Internal error. Slot ~A has no #$Inverse value!" slot))
  (unless (framep inverse-slot)
    (error 
     (one-string
      "Ruh Roh. Internal error. #$Inverse value, ~A, for slot ~A, "
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
        (inverse-slot (slotv slot #$inverse)))
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

;; This probably isn't necessary

(defun generic-inverse-slot-add-daemon 
       (frame slot new-element prevent-duplicates?)
  (let ((inverse-slot (slotv slot #$inverse)))
    (verify-inverse-slot inverse-slot slot)
    (print (list 'add 'frame frame 'slot slot 'inverse inverse-slot))
    (unless (framep new-element)
      (slot-must-contain-only-frames frame slot inverse-slot new-element))
    (maybe-adjust-inverse-slot 
     frame new-element inverse-slot prevent-duplicates?)
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

  (push #$CPU (slotv #$COMPUTER #$PARTS))

  to be executed.

  By defining PARTS and PARTOF to be inverses using the statement

  (DEF-INVERSE-SLOT PARTS PARTOF)

  we cause this to happen.  (Note that the inverse does NOT happen!  If we
  were to execute the code we want to happen automatically, the original code
  we used would not get triggered.  This seems bogus.)
  
|#

(defmacro def-inverse-slot (inverse-slot slot &key (prevent-duplicates? nil))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (create-inverse-slot
      ,inverse-slot ,slot
      (lambda (frame slot new-list)
        (generic-inverse-slot-put-daemon
         frame slot new-list ,prevent-duplicates?))
      (lambda (frame slot new-list)
        (generic-inverse-slot-add-daemon
         frame slot new-list ,prevent-duplicates?))
      )))

(defun create-inverse-slot (inverse-slot slot pf af)
  (let ((put-function-name 
         (derived-slot-function-name slot "INVERSE" "PUT-DAEMON"))
        (add-function-name
         (derived-slot-function-name slot "INVERSE" "ADD-DAEMON"))
        (cpf (if (compiled-function-p pf) pf (compile nil pf)))
        (caf (if (compiled-function-p af) af (compile nil af))))
    (defslot inverse-slot :set-valued? t)
    (defslot slot :set-valued? t)
    (setf (slotv inverse-slot #$inverse) slot)
    (setf (slotv slot #$inverse) inverse-slot)
    (setf (symbol-function put-function-name) cpf)
    (setf (symbol-function add-function-name) caf)
    (add-put-demon slot put-function-name)
    (add-add-demon slot put-function-name)
    (values add-function-name put-function-name)
    ))


(defmacro def-reciprocal-inverse-slots 
          (slota slotb &key (prevent-duplicates? nil))
  `(progn 
     (def-inverse-slot ,slota ,slotb :prevent-duplicates? ,prevent-duplicates?)
     (def-inverse-slot ,slotb ,slota :prevent-duplicates? ,prevent-duplicates?)
     ))

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

;;; @@@
(defun recursive-print 
       (start child-generator &optional (print-function #'princ))
  (recursive-descent 
   start child-generator
   (lambda (node)
     (terpri)
     (dotimes (n *recursive-descent-level*) (princ "  "))
     (funcall print-function node))))

;;; @@@
(defun print-hierarchy (start slot)
  (recursive-print 
   start 
   (slot-accessor slot)
   (lambda (f) (format t "~A: ~A" f (slotv f #$definition)))))

;;; Inheriting slots

(defun def-inherited-slot (slot &optional (inherits-through #$isA))
  #.(one-string-nl
     "define a SLOT whose value, if it does not exist, is computed from "
     "the SLOT value of one of the frames it inherits through, transitively "
     "(inheritance defaulting to an ISA relationship).  If more than "
     "one frame is a direct parent of SLOT via the inheritance relationship "
     "the search for a non-nil value proceeds depth-first not breath-first. ")
  (defslot slot)
  (setf (slotv slot #$getter) 'inheriting-get)
  (setf (slotv slot #$inheritsThrough) inherits-through))

(defun inheriting-get (frame slot)
  (or (%slotv frame slot)
      ;; does depth-first search of parents, not the right thing.
      (let ((parents (slotv frame (slotv slot #$inheritsThrough))))
	(dolist (parent parents nil)
	  (vwhen (inherited-value (slotv parent slot)) 
            (return inherited-value)
            )))))


;;; Support for temporary frames

#|
Semantic note: temp frames can have an #$instanceOf slot that will help them 
generate a name. The problem with this is that if we someday add a real 
instance ontology, we might generate inverse pointers. This would result 
in the frames not being temporary since they will be referenced by their 
class frame. So the #$instanceOf slot used here might want to become 
something more special-purpose.
|#


;;; Interns a temp frame, returning the new gensymed name. 
;;; Note this doesn't actually set the name since the computed slot
;;; mechanism will do that.
(defun gensym-and-intern (temp-frame)
  (let ((class-frame (or (slotv temp-frame #$instanceOf) #$Temp)))
    (unless (slotv class-frame #$gensymCounter)
      (setf (slotv class-frame #$gensymCounter) 0))
    (let* ((num (incf (slotv class-frame #$gensymCounter)))
	   (name (one-string (slotv class-frame #$fName) 
                             (princ-to-string num))))
      (if (frame-fnamed name)
	  (gensym-and-intern temp-frame) ;name already exists, recurse
	;; no longer temp, it has a name
	(progn (intern-frame temp-frame name)
	       name)))))


;;; redefine to do full slot access on fname
(defun print-frame (frame stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#$~A" (slotv frame #$fName)))

;;; Call this to make a temp frame
(defun make-temp-frame (&optional class)
  #.(one-string-nl
     "Create a frame which is not interned.  It is not put in the table "
     "of all accessible frames.")
  (let ((f (make-%frame)))
    (when class (setf (slotv f #$instanceOf) class))
    f))


(defgeneric top-level-frame? (application frame))

(defmethod top-level-frame? ((application t) frame) 
  (declare (ignore frame))
  nil)

(defun top-level-frames (&aux result)
  (for-all-frames 
   (f)
   (unless 
       (or (slotv f #$isa)
           ;; The GO ONTOLOGY uses PARTS and PARTOF
           (slotv f #$partof)
           ;; INSTANCEOF only used by temporary frame mechanism currently, so
           ;; FOR-ALL-FRAMES won't access such frames anyway.  But leave code
           ;; here in case semantics of INSTANCEOF changes.
           (slotv f #$instanceOf)
           ;; Applications can determine for themselves whether
           ;; something is a top level frame; e.g. Organism-Entity-Type
           (top-level-frame? 
            (symbol-value-in-package :*application* :cl-user :if-does-not-exist t)
            f
            ))
     (push f result)))
  result)

(defun top-level-ancestors ()
  (remove-if-not
   (lambda (f) (or (slotv f #$subClasses) (slotv f #$parts)))
   (top-level-frames)))


;;; Why not just call MAKE-TOP instead of putting it in the
;;; application initializations?  Because it wouldn't find all of
;;; the application's top level frames if it were called before
;;; all the application code and its initializations were completed.

(defun make-top ()
  (def-frame "Top"
      #$description 
      (one-string 
       "This is the top frame of the knowledge base. Its children include "
       "the top frames of imported ontologies."))
  (dolist (frame (top-level-ancestors)) 
    (unless (eq frame #$Top) (pushnew #$Top (slotv frame #$isA)))
    ))


