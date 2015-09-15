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

(defvar *frames-safety* nil)

;; Now we assume readtable is set up, so we can use #$ and #^. 

;;; High-level slot access

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

(defmethod slotv ((frame aframe) (slot t))

  #.(one-string-nl
     "Access the value of SLOT within FRAME.  This form is setf-able."
     "A second value is returned which is T if the slot actually"
     "exists, and NIL if it does not exist (The first value will be NIL"
     "if either the value of the slot is NIL, or the slot does not exist.")

  (internal-slotv frame slot)

  )

(defun internal-slotv (frame slot)
  (when *frames-safety*
    (unless (isframe? slot) (bad-slot-access frame slot)))
  (cond
   ((eq slot *fname-frame*) (values (fname frame) t))
   ((eq slot *iname-frame*) (values (iname frame) t))
   (t
    (multiple-value-bind (value exists?) 
        (%slotv frame slot)
      (if exists? 
          (values value t)
        (inheriting-slotv frame slot)
        )))))

(defun inheriting-slotv (frame slot)
  (let ((frame-type (type-of frame)))
    (if (eq frame-type 'aframe)
        (values nil nil)
      (let ((type-frame (frame-fnamed (string frame-type))))
        (if (null type-frame)
            (values nil nil)
          (multiple-value-bind (value exists?)
              (%slotv type-frame slot)
            (if exists? (values value :inherited) (values nil nil))
            ))))))

(defmethod set-slotv ((frame aframe) (slot t) value)

  #.(one-string-nl
     "Store VALUE as the value of SLOT within FRAME."
     "Assuming no errors, the SLOT's value is set to VALUE and VALUE is"
     "returned as the first value, while either :NEW or :EXISTING is returned"
     "as the 2nd value, depending on whether the slot was added to the frame"
     "or was already there.")

  (when *frames-safety*  
    (unless (isframe? frame) (bad-slot-access frame slot))
    (unless (isframe? slot) (bad-slotv-call frame)))

  (when (eq slot *fname-frame*) 
    (error "You cannot change a frame's fName!"))

  (when (eq slot *iname-frame*) 
    (error "You cannot change a frame's iName!"))

  (%set-slotv frame slot value)

  )

(defsetf slotv set-slotv)

(defmethod delete-slot ((frame aframe) (slot t))
  #.(one-string-nl
     "Removes SLOT from FRAME (as opposed to just setting its"
     "value to NIL, say.)  Once this is called, the function"
     "(FRAME-HAS-SLOT? FRAME SLOT) will return NIL."
     "(Note that (SLOTV FRAME SLOT) will return NIL whether a slot exists"
     "or the slot value is NIL.)")
  (unless (isframe? frame) (error "Not a frame: ~A" frame))
  (unless (isframe? slot) (error "Not a frame: ~A" slot))
  (when (eq slot *fname-frame*)
    (error "You cannot delete the #$Fname slot of a frame!"))
  (when (eq slot *iname-frame*)
    (error "You cannot delete the #$iName slot of a frame!"))
  (%remove-slot frame slot) 
  frame
  )

;;; shorthand for functional access
(defun slot-accessor (slot) (lambda (frame) (slotv frame slot)))

(defmethod describe-frame ((frame aframe))
  "Print the frame object name and value for every frame slot of FRAME"
  (if (not (isframe? frame))
      (progn
        (warn "~A is not frame object!  Using DESCRIBE..." frame)
        (describe frame))
    (progn
      (terpri)
      (format t "~&Slots of ~A (of type ~A):" frame (type-of frame))
      (let ((slots-and-values nil)
            (current-slot nil))
        (handler-case
            (progn
              (for-each-frame-slot
               (slot value) frame
               (unless (eq slot *fname-frame*)
                 (push (list slot value) slots-and-values)))
              (setq slots-and-values 
                    (sort slots-and-values 
                          'string-lessp :key (lambda (x) (fname (first x)))))
              (loop for (slot value) in slots-and-values do
                    (setq current-slot slot)
                    (format t "~&  ~S:~8T~S" slot value)
                    ))
          (error 
           ()
           (formatt 
            (one-string-nl 
             ""
             "ERROR: There is undisplayable content in this frame."
             "The slot containing the undisplayable content is ~A"
             )
            current-slot
            ))))))
  (terpri)
  (values)
  )

(defmethod describe-object ((frame aframe) stream)
  (let ((*standard-output* stream)) (describe-frame frame)))

(defun df (frame) 
  "Describe (pretty print) a frame's slots and their values"
  (describe-frame frame))

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

(defmethod delete-frame ((frame aframe))
  #.(one-string-nl 
     '("Actually deletes a frame and removes it from the acache database."
       "Must commit to really get rid of the acache object in a real acache."
       "WARNING: If there are any slots or other references to this frame, an"
       "acache error will be signaled in real acache each time anyone tries to"
       "access the frame (e.g., the slot)."
       "Therefore, this is likely to break everything!"
       ))
  (%delete-frame frame)
  t)

(defun unintern-frame (frame) (delete-frame frame))

(defun frame-print-prefix (frame) (declare (ignore frame)) "#$")
  
(defun print-frame (frame stream &rest ignore)
  (declare (ignore ignore))
  (format stream "~A~A" (frame-print-prefix frame) (fname frame))
  )


;;; Abstraction for slots whose value is a list.

(defun add-element (frame slot elt &key (test 'eql))
  (pushnew elt (slotv frame slot) :test test)
  elt)

(defun delete-element (frame slot elt &key (test 'eql))
  "Remove the element ELT from the value of SLOT in FRAME"
  (setf (slotv frame slot) (delete elt (slotv frame slot) :test test)))

(defun has-element? (frame slot elt &key (key 'identity) (test 'eql))
  "Non-NIL if the element ELT is included in the value of SLOT in FRAME"
  (member elt (slotv frame slot) :key key :test test))


