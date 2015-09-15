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

;;; The second part of the frame system. 
;;; Mike Travers, 2003/4, JP Massar 9/2004

(defparameter *set-valued-non-list-action* :warn-and-convert)

;; Now we assume readtable is set up, so we can use #$ and #^. 

(declaim (inline fname))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun fname (frame) (%slotv frame #$Fname))

  (defun fstring (x) 
    "Generalization of STRING function to include FRAME objects"
    (if (framep x) (fname x) (string x)))

  )

(defun bad-slot-access (frame slot)
  (error
   (one-string-nl
    "You are trying to access or set a component of the frame ~A,"
    "but components of frames are referenced by other frames."
    "You used ~S as the reference, which is a ~S, not a frame.")
   frame slot (type-of slot)))

(defun bad-slotv-call (non-frame)
  (error
   (one-string-nl
    "You are trying to use SLOTV to access or set ~S, but that"
    "function can only be used with frames.  You called it with ~S,"
    "which is of type ~S.")
   non-frame non-frame (type-of non-frame)))


;;; High-level slot access

(defun slotv (frame slot)
  #.(one-string-nl
     "Access the value of SLOT within FRAME.  This form is setf-able."
     "If SLOT has a non-nil value for it's own #$getter slot, that value"
     "is assumed to be a function, and that function is called with"
     "SLOTV's FRAME and SLOT arguments to compute a value to be returned"
     "instead of retrieving the value directly.")
  (unless (isframe? frame) (bad-slotv-call frame))
  (unless (isframe? slot) (bad-slot-access frame slot))
  (cond
   ;; Special case #$Fname so we don't have to retrieve
   ;; the getter function all the time and call it; we just retrieve
   ;; the name if it exists.
   ((eq slot #$Fname)
    (or (%slotv frame #$Fname) (funcall (%slotv slot #$getter) frame slot)))
   (t
    (vif (access-function (%slotv slot #$getter))
         (funcall access-function frame slot)
         (%slotv frame slot)
         ))))

(defun frame-slot-value (frame slot) 
  "An alias for (slotv frame slot).  The value of SLOT within FRAME."
  (slotv frame slot))

(defun handle-non-list-for-set-valued-slot (frame slot value)
  (ecase *set-valued-non-list-action*
    (:error 
     (one-string
      (error
       "Attempt to set set-valued slot, ~A, of frame ~A, to a non-list "
       "value, ~A.")
      slot frame value))
    (:warn-and-convert
     (warn 
      (one-string
       "Set-valued slot, ~A, being set to non-list value ~A, for frame "
       "A singleton list of the value will be stored instead.")
      slot frame value)
     (list value))
    (:convert-silently (list value))
    ))

(defun set-slotv (frame slot value)
  #.(one-string-nl
     "Store VALUE as the value of SLOT within FRAME."
     "If SLOT has a non-nil value for it's own #$putDemons slot, it is"
     "assumed to be a list of functions of three arguments, each of which"
     "is called in list order.  The arguments passed to each function are"
     "SET-SLOTV's FRAME, SLOT and VALUE arguments.  These functions are"
     "called for side effect only.  Regardless of whether any functions"
     "are called, the SLOT's value is set to VALUE.")
  (unless (isframe? frame) (bad-slotv-call frame))
  (unless (isframe? slot) (bad-slot-access frame slot))
  ;; (print (list 'frame frame 'slot slot 'value value))
  (loop for demon in (slotv slot #$putDemons) do
        ;; (print (list 'demon demon))
        (funcall demon frame slot value))
  (when (%slotv slot #$SetValued)
    (when (not (listp value))
      (setq value (handle-non-list-for-set-valued-slot frame slot value))
      ))
  (%set-slotv frame slot value))

(defsetf slotv set-slotv)
(defsetf frame-slot-value set-slotv)
(defsetf slot-value-of-frame set-slotv)

(defun delete-slot (frame slot)
  #.(one-string-nl
     "Removes SLOT from FRAME (as opposed to just setting its"
     "value to NIL, say.)  Once this is called, the function"
     "(FRAME-HAS-SLOT? FRAME SLOT) will return NIL."
     "(Note that (SLOTV FRAME SLOT) will return NIL whether a slot exists"
     "or the slot value is NIL.)")
  (unless (isframe? frame) (error "Not a frame: ~A" frame))
  (unless (isframe? slot) (error "Not a frame: ~A" slot))
  (%remove-slot frame slot)
  frame
  )


;;; shorthand for functional access
(defun slot-accessor (slot) (lambda (frame) (slotv frame slot)))

(defun describe-frame (frame &optional (compute-slots? nil))
  #.(one-string-nl
     "Print the frame object name and value for every frame slot of FRAME"
     "Computed slots which have not yet been computed are NOT computed here"
     "unless COMPUTE-SLOTS? to T (the default is NIL).  Slots are"
     "printed out in alphabetical order by name of slot (case insensitive).")
  (if (not (isframe? frame))
      (progn
        (warn "~A is not frame object!  Using DESCRIBE..." frame)
        (describe frame))
    (progn
      (if (%slotv frame #$fname)
          (format t "~&Slots of ~A:" frame)
        (format t "~&Slots of temp frame:"))
      (let ((slots-and-values nil))
        (for-each-frame-slot (slot value) frame
          (unless (eq slot #$Fname)
            (let* ((slot-types (slotv slot #$isA))
                   (real-value
                    (cond
                     ((null slot-types) value)
                     ((member #$AlwaysComputedSlot slot-types)
                      (slotv frame slot))
                     ((member #$ComputedSlot slot-types)
                      (if compute-slots? (slotv frame slot) value))
                     (t value)
                     )))
              (push (list slot real-value) slots-and-values))))
        (setq slots-and-values 
              (sort slots-and-values 
                    'string-lessp :key (lambda (x) (fname (first x)))))
        (loop for (slot real-value) in slots-and-values do
              (format t "~&~S:~8T~S" slot real-value)
              )))))

(defmethod describe-object ((frame %frame) stream)
  (let ((*standard-output* stream)) (describe-frame frame)))

(defun df (frame &optional (compute-slots? nil))
  "Describe (pretty print) a frame's slots and their values"
  (describe-frame frame compute-slots?))

;;; define a frame plus some slots
(defun def-frame (frame &rest slots)
  #.(one-string-nl
     "If FRAME does not yet exist create it. (FRAME may be a string or symbol"
     "naming a frame).  SLOTS is a list of the form:"
     "(<slot1> <value1> <slot2> <value2> ... <slotn> <valuen>)"
     "For each corresponding slot and value in the list, FRAME is made to"
     "explicitly have that slot storing that value."
     "The existing or newly created frame is returned.")
  ;; Coerce FRAME to be a frame, explicitly
  (setq frame (frame-fnamed (fstring frame) t))
  ;; fill in slots
  (do ((rest-slots slots (cddr rest-slots))) ((null rest-slots))
    (setf (slotv frame (car rest-slots)) (cadr rest-slots)))
  frame)

(defun rename-frame (frame new-name &key (if-new-frame-exists :error))
  #.(one-string-nl
     "Give FRAME a new name, NEW-NAME.  If a frame named NEW-NAME already"
     "exists IF-NEW-FRAME-EXISTS determines what happens.  Possibilities are:"
     "  :ERROR -- an error is signalled."
     "  :WARN  -- a warning is issued; the frame is not renamed"
     "  :REPLACE -- if a frame NEW-NAME exists, it is uninterned, and FRAME"
     "     replaces it."
     "  NIL -- The frame is not renamed, silently."
     "FRAME is always returned (with its #$fName possibly changed) but with"
     "all its other slots intact.")
  (flet ((do-it ()
           (unintern-frame frame)
           (setf (%slotv frame #$fName) new-name)
           (intern-frame frame new-name)
           ))
    (let ((existing-frame (frame-fnamed new-name)))
      (if existing-frame
          (ecase if-new-frame-exists
            (:error (error "There already is a frame named ~A" new-name))
            (:warn 
             (warn "A frame named ~A already exists! Frame ~A not renamed."
                   new-name frame))
            (:warn-and-replace
             (warn "A frame named ~A already exists! It will be uninterned."
                   new-name)
             (unintern-frame existing-frame)
             (do-it))
            ((:overwrite :replace) 
             (unintern-frame existing-frame)
             (do-it))
            ((nil) frame))
        (do-it))
      frame
      )))
      

;;; Abstraction for slots whose value is a list.

;;; This is backwards from the semantics of SET-SLOTV
;;; In SET-SLOTV the daemons are called first, then the new value
;;; is set.  Here the element is first pushnew'ed, then the daemons
;;; are called.

;;; Element API


;; This works the standard way.

(defun add-element (frame slot elt &key (test 'eql))
  (pushnew elt (slotv frame slot) :test test)
  elt)

(defun delete-element (frame slot elt &key (test 'eql))
  "Remove the element ELT from the value of SLOT in FRAME"
  (setf (slotv frame slot) (delete elt (slotv frame slot) :test test)))

(defun has-element? (frame slot elt &key (key 'identity) (test 'eql))
  "Non-NIL if the element ELT is included in the value of SLOT in FRAME"
  (member elt (slotv frame slot) :key key :test test))



(defun delete-frame-contents (frame)
  "Removes all the slots of FRAME except for its FNAME."
  (let ((fname (%slotv frame #$Fname)))
    (setf (%frame-slots frame) nil)
    (setf (%slotv frame #$Fname) fname)
    frame
    ))
