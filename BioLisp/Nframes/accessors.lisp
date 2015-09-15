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

(defun frame-slot-value (frame slot) 
  "An alias for (slotv frame slot).  The value of SLOT within FRAME."
  (slotv frame slot))

(defun slot-value-of-frame (slot frame) 
  "An alias for (slotv frame slot).  The value of SLOT within FRAME."
  (slotv frame slot))

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

;;; New SLOTV algorithm

#||

IF the frame is generic
   THEN if the slot is generic
        THEN do a standard access on the existing slots,
             returning (values NIL NIL) if not found.
        ;; the slot has some properties
        ELSEIF the slot is always computed
          THEN perform the computation and return value
        ELSEIF the slot if computed
          THEN if no slot exists
               THEN do the computation and store result
               ELSE return value of found slot
        ELSEIF the slot is inherited
          THEN try standard accesss (the slot is physically present)
               and if that fails try calling the inheritance code
               using the %inherited-slotv method on normal frames.
        ELSEIF <some other property as yet undefined>
          THEN deal with it
        ELSE <internal error>, what else could it be?
   ELSE (the frame is not generic)
     THEN
       test the frame permissions, error if not allowed access.
       test the autoload, if enabled do the autoload and recurse
       do whatever else might be appropriate
       IF the frame is typed 
          THEN
           IF the slot is a fixed slot of the frame type,
             THEN retrieve the value
             ELSEIF the slot is always computed
               THEN perform the computation and return value
             ELSEIF the slot if computed
               THEN if no slot exists
                 THEN do the computation and store result
                 ELSE return value of found slot
             ELSEIF the slot is present but not fixed         
               THEN return its value
             ELSE
               Try inheritance based on the CLOS class structure,
               including dealing with CLASS slots,
               and return value if found, otherwise (values NIL NIL)
           ELSE do standard access on existing slots
                returning (values NIL NIL) if not found.

||#


(defun slotv (frame slot)

  #.(one-string-nl
     "Access the value of SLOT within FRAME.  This form is setf-able."
     "If the slot does not exist, the value is determined in various"
     "ways depending on the FRAME's type:  e.g., the value may be"
     "inherited from from the frame type's frame class, or it may be"
     "computed, or if all else fails NIL is returned."
     "A second value is returned which is T if the slot actually"
     "exists, and NIL if it does not exist (The first value will be NIL"
     "if either the value of the slot is NIL, or the slot does not exist.")

  (when *frames-safety*
    (unless (isframe? frame) (bad-slot-access frame slot))
    (unless (isframe? slot) (bad-slotv-call frame)))

  (block exit
    
    ;; Short circuit #$Fname since it is special and common.
    (when (eq slot *fname-frame*)
      (return-from exit (values (fname frame) t)))

    (macrolet ((return-if-found (form)
                 `(multiple-value-bind (rval found?)
                      ,form
                    (when found? (return-from exit (values rval t)))
                    )))

      (let ((frame-info (%aframe-info frame))
            (slot-info (%aframe-info slot))
            (slotv-frame-flags-mask 
             #.(multimask 
                '(:frame-permissions :frame-autoload? :frame-typed?)))
            (slotv-slot-flags-mask
             #.(multimask
                '(:slot-computed :slot-always-computed :slot-inherited))))
        (declare (fixnum frame-info slot-info))
        (declare (fixnum slotv-frame-flags-mask slotv-slot-flags-mask))
          
        (if (zerop (logand frame-info slotv-frame-flags-mask))

            ;; The frame is generic

            (cond
             ;; The slot is generic
             ((zerop (logand slot-info slotv-slot-flags-mask))
              (%simple-slotv frame slot))
             ((flagon? slot-info :slot-always-computed)
              (%always-computed-slotv frame slot))
             ((flagon? slot-info :slot-computed)
              (%computed-slotv frame slot))
             ((flagon? slot-info :slot-inherited)
              (return-if-found (%simple-slotv frame slot))
              (%inherited-slotv frame slot))
             (t (error "Internal error. Slot flags and mask out of sync.")))

          ;; The frame is not generic.
          ;; Check for bits signalling special frame handling.
          (progn
            (when (flagon? frame-info :frame-permissions)
              (check-frame-access-permissions frame frame-info))
            (when (flagon? frame-info :frame-autoload?)
              (return-from exit
                (progn (autoload-frame frame) (slotv frame slot))))
            (when (flagon? frame-info :frame-typed?)
              ;; The frame is typed.  Is the slot one of its fixed slots?
              (return-if-found (%fixed-slotv frame slot))
              ;; No.  Is the slot defined as computed?
              (cond
               ((flagon? slot-info :slot-always-computed)
                (return-from exit (%always-computed-slotv frame slot)))
               ((flagon? slot-info :slot-computed)
                (return-from exit (%computed-slotv frame slot)))
               ;; No. 
               (t 
                ;; Is it a normal, fixed slot?
                (return-if-found (%simple-slotv frame slot))
                ;; No, Try CLOS inheritance, then give it up.
                (return-from exit (%inherited-slotv frame slot))
                )))
            (%simple-slotv frame slot))

          )))))


(defun %simple-slotv (frame slot)
  (let* ((obj (%aframe-slots frame))
         (fslot (and obj (%find-fslot obj slot))))
    (if (null fslot)
        (values nil nil)
      (let ((fslot-info (%fslot-info fslot)))
        (when (flagon? fslot-info :frame-permissions)
          (check-fslot-access-permissions frame fslot))
        (values (%fslot-value fslot) t)
        ))))


;;; No fixed slots for standard frames.
(defmethod %fixed-slotv ((frame %aframe) slot)
  (declare (ignorable slot))
  (error 
   "Internal error. %fixed-slotv called with generic frame & slot: ~A, ~A."
   frame slot
   ))

;;; No inheritance mechanism yet for standard frames.
(defmethod %inherited-slotv ((frame %aframe) slot)
  (inheriting-get frame slot)
  )

(defun %computed-slotv (frame slot)
  (multiple-value-bind (rval found?)
      (%simple-slotv frame slot)
    (if found?
        (values rval t)
      (vif (f (%simple-slotv slot #$sys.getter))
           ;; The computed-slot function always stores
           ;; the computed value into the slot so it is cached.
           (values (funcall f frame slot) t)
           (values nil nil)
           ))))

(defun %always-computed-slotv (frame slot)
  (vif (f (%slotv slot #$sys.getter))
       (values (funcall f frame slot) :always-computed)
       (error 
        #.(one-string-nl
           "Inconsistency: Slot ~A has ALWAYS-COMPUTED flag set but"
           "no #$SYS.GETTER slot value..."
           ))))

(defun check-frame-access-permissions (frame frame-info)
  (declare (ignore frame frame-info))
  (error "Not implemented."))

(defun autoload-frame (frame)
  (declare (ignore frame))
  (error "Not implemented."))

(defun check-fslot-access-permissions (frame fslot)
  (declare (ignore frame slot fslot))
  (error "Not implemented."))
       


(defun set-slotv (frame slot value)

  #.(one-string-nl
     "Store VALUE as the value of SLOT within FRAME."
     "The FRAME or SLOT may be READ-ONLY in which case an error"
     "will be signalled."
     "The SLOT may be restricted to hold only certain kinds of values,"
     "and if so, and if VALUE does not satisfy the constraint, an error"
     "will be signalled."
     "Under certain conditions, if the FACET holding the existing value, or"
     "the SLOT frame itself, has a non-nil value for it's own #$putDemons slot,"
     "it is assumed to be a list of functions of three arguments, each of"
     "which is called in list order.  The arguments passed to each function"
     "are SET-SLOTV's FRAME, SLOT and VALUE arguments.  These functions are"
     "called for side effect only."
     "Assuming no errors, the SLOT's value is set to VALUE and VALUE is"
     "returned as the first value, while either :NEW or :EXISTING is returned"
     "as the 2nd value, depending on whether the slot was added to the frame"
     "or was already there.")

  (when *frames-safety*  
    (unless (isframe? frame) (bad-slot-access frame slot))
    (unless (isframe? slot) (bad-slotv-call frame)))

  (when (eq slot *fname-frame*) 
    (error "You cannot change a frame's fname!"))

  ;; Get the bits pertaining to 'setting' from the frame

  (let ((frame-info (%aframe-info frame))
        (set-slotv-frame-mask 
         #.(multimask '(:frame-permissions :frame-typed?)))
        (slot-state nil))
    (declare (fixnum frame-info slotv-flags-mask))

    (if (zerop (logand frame-info set-slotv-frame-mask))

        ;; No frame flag bits are set.

        (setq slot-state (%set-slotv-no-frame-flags frame slot value))

      ;; Frame flag bits are set

      (progn
        (when (flagon? frame-info :frame-permissions)
          (check-frame-write-permissions frame frame-info))
        (if (not (flagon? frame-info :frame-typed?))
            (setq slot-state (%set-slotv-no-frame-flags frame slot value))
          (if (%fixed-set-slotv frame slot value)
              (setq slot-state :existing)
            (setq slot-state (%non-fixed-set-slotv frame slot value))
            ))))

    (assert-frame-modified frame)

    (values value slot-state)

    ))

(defun assert-frame-modified (frame)
  ;; Setting the modified? bit here insures that AllegroCache
  ;; will eventually write the change to persistent storage
  ;; when we do a commit.  The problem is that a %set-slotv
  ;; operation will not usually change the %aframe-slots value
  ;; value, it will often only change the value of a cons cell
  ;; internal to the frame structure.
  (clear-frame-flag frame :frame-modified?)
  (enable-frame-flag frame :frame-modified?)
  (let ((obj (%aframe-slots frame)))
    (setf (%obj-value obj) (%obj-value obj)))
  (when *after-frame-modification-hook* 
    (funcall *after-frame-modification-hook* frame)
    ))


(defun %slot-frame-check (frame slot value)
  (let ((sframe-info (%aframe-info slot))
        (set-slotv-sframe-mask #.(multimask '(:slot-domain :slot-change))))
    (when (plusp (logand sframe-info set-slotv-sframe-mask))
      (progn
        (when (flagon? sframe-info :slot-domain)
          (verify-sframe-domain frame slot sframe-info value))
        (when (flagon? sframe-info :slot-change)
          (execute-slot-change-demons frame slot value)
          )))))


(defun %set-slotv-no-frame-flags (frame slot value)
  (let* ((obj (%aframe-slots frame))
         (fslot (and obj (%find-fslot obj slot)))) 
    ;; Does the fslot for this slot/value already exist?
    (if (null fslot)
        ;; It does not exist.  Add it after checking the slot frame.
        (progn
          (%slot-frame-check frame slot value)
          (%add-an-fslot frame slot value)
          :new)
      ;; It does exist.  Add it after checking the fslot bits and
      ;; the slot frame.
      (let ((fslot-info (%fslot-info fslot))
            (set-slotv-fslot-mask
             #.(multimask 
                '(:fslot-permissions :fslot-domain 
                  :fslot-change :fslot-no-slot-check?))))
        (if (zerop (logand fslot-info set-slotv-fslot-mask))
            (progn 
              (%slot-frame-check frame slot value)
              (%set-existing-fslot fslot value))
          (progn
            (when (flagon? fslot-info :fslot-permissions)
              (check-fslot-write-permissions frame fslot))
            (when (flagon? fslot-info :fslot-domain)
              (verify-fslot-domain frame fslot fslot-info value))
            (when (flagon? fslot-info :fslot-change)
              (execute-fslot-change-demons frame fslot value))
            (if (flagon? fslot-info :fslot-no-slot-check?)
                (%set-existing-fslot fslot value)
              (progn
                (%slot-frame-check frame slot value)
                (%set-existing-fslot fslot value)
                ))))
        :existing
        ))))


(defmethod %non-fixed-set-slotv ((frame %aframe) slot value)
  (declare (ignorable slot value))
  (error "Should never call %non-defined-set-slotv on %aframe: ~A" frame))

(defmethod %fixed-slot? ((frame %aframe) slot)
  (declare (ignorable slot))
  (error "Should never call %fixed-slotv on %aframe: ~A" frame)) 

(defmethod %fixed-set-slotv ((frame %aframe) slot value)
  (declare (ignorable slot value))
  (error "Should never call %fixed-set-slotv on %aframe: ~A" frame))

(defun check-frame-write-permissions (frame fslot)
  (declare (ignore frame fslot))
  (error "Not implemented."))

(defun check-fslot-write-permissions (frame fslot)
  (declare (ignore frame fslot))
  (error "Not implemented."))

(defun execute-slot-change-demons (frame slot value)
  (loop for demon in (slotv slot #$sys.putDemons) do
        (if (symbolp demon)
            (if (not (fboundp demon))
                (warn "No function for ~A for slot ~A defined." demon slot)
              (funcall demon frame slot value))
          (progn 
            (warn "Actual function object as demon for slot ~A!!" slot)
            (funcall demon frame slot value)
            ))))

(defun execute-fslot-change-demons (frame fslot value)
  (loop for demon in (getf (%fslot-slot fslot) #$sys.putDemons) do
        (if (symbolp demon)
            (if (not (fboundp demon))
                (warn "No function for ~A for slot ~A defined." demon fslot)
              (funcall demon frame (%fslot-slot fslot) value))
          (progn 
            (warn "Actual function object as demon for slot ~A!!" fslot)
            (funcall demon frame (%fslot-slot fslot) value)
            ))))

(defun verify-sframe-domain (frame slot slot-info value)
  (flet ((oops (dp &optional desc)
           (error 
            (formatn
             (one-string-nl
              "Attempt to set ~A slot of ~A frame failed because: "
              "the value being set, ~S, does not satisfy the Domain Predicate"
              "constraint (~S) for that slot.")
             slot frame value (or desc dp)
             ))))
    (cond 
     ((flagon? slot-info :slot-domain-list?)
      (unless (listp value) (oops "it is not a list")))
     ((flagon? slot-info :slot-domain-set?)
      (unless (listp value) (oops "it is not a set")))
     (t 
      (let ((dp (slotv slot #$sys.DomainPredicate)))
        (unless dp (error "Internal error. Domain bit set but no predicate."))
        (unless (funcall dp value) (oops dp))
        )))))

(defun verify-fslot-domain (frame fslot fslot-info value)
  (declare (ignore slot-info))
  (flet ((oops (dp &optional desc)
           (error 
            (formatn
             (one-string-nl
              "Attempt to set ~A slot of ~A frame failed because: "
              "the value being set, ~S, does not satisfy the Domain Predicate"
              "constraint (~S) for the particular fslot of that frame.")
             (%fslot-slot fslot) frame value (or desc dp)
             ))))
    (cond 
     ;; Replace these with bit tests!
     ((flagon? fslot-info :fslot-domain-list?)
      (unless (listp value) (oops "it is not a list")))
     ((flagon? fslot-info :fslot-domain-set?)
      (unless (listp value) (oops "it is not a set")))
     (t 
      (let ((dp (getf (%fslot-properties fslot) #$sys.DomainPredicate)))
        (unless dp (error "Internal error. Domain bit set but no predicate."))
        (unless (funcall dp value) (oops dp))
        )))))


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
  (when (eq slot *fname-frame*)
    (error "You cannot delete the #$Fname slot of a frame!"))
  (let ((frame-info (%aframe-info frame)))
    (when (flagon? frame-info :frame-permissions)
      (check-frame-slot-delete-permissions frame slot)))
  (when (%remove-slot frame slot) (assert-frame-modified frame))
  frame
  )

(defun check-frame-slot-delete-permissions (frame slot)
  (declare (ignore frame slot))
  (error "Not implemented!"))


(defun verify-frame-is-purgeable (frame)
  (unless (isframe? frame) (error "~A is not a frame!" frame))
  (let ((frame-info (%aframe-info frame)))
    (when (flagon? frame-info :frame-permissions)
      (check-frame-write-permissions frame frame-info))))

(defun purge-frame 
       (frame &key (reset-timestamp? t) (reinitialize-fixed-slots? t))
  #.(one-string-nl
     "Clears out the slots of FRAME either by removing the slots completely"
     "(in the case that the slot is not fixed), or reinitializing a slot"
     "(in the case that the slot is fixed).  The #$Fname and #$Prettyname"
     "slots are not modified.  If RESET-TIMESTAMP? is T, then the frame's"
     "timestamp is reset to the current time, otherwise it is preserved."
     "If REINITIALIZE-FIXED-SLOTS? is NIL (default T) then the fixed slots of"
     "the frame are not reinitialized, rather they are left as they were.")
  (verify-frame-is-purgeable frame)
  (when (%purge-frame frame reset-timestamp? reinitialize-fixed-slots?)
    (assert-frame-modified frame))
  frame)

;;; shorthand for functional access
(defun slot-accessor (slot) (lambda (frame) (slotv frame slot)))

(defun describe-frame (frame &key (bits? t) (compute-slots? nil))
  (declare (ignore compute-slots?))
  #.(one-string-nl
     "Print the frame object name and value for every frame slot of FRAME"
     "Computed slots which have not yet been computed are NOT computed here"
     "unless COMPUTE-SLOTS? to T (the default is NIL).")
  (if (not (isframe? frame))
      (progn
        (warn "~A is not frame object!  Using DESCRIBE..." frame)
        (describe frame))
    (progn
      (terpri)
      (if (not (temporary-frame? frame))
          (if (eq (type-of frame) '%aframe)
              (format t "~&Slots of ~A:" frame)
            (format t "~&Slots of ~A (of type ~A):" frame (type-of frame)))
        (format t "~&Slots of temp frame ~A:" frame))
      (let ((slots-and-values nil))
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
                    (format t "~&  ~S:~8T~S" slot value)))
          (error 
           ()
           (formatt (one-string-nl 
                     ""
                     "ERROR: There is undisplayable content in this frame."
                     "       (This could be the result of having deleted a slot's frame"
                     "        using (delete-frame...).)" 
                     "")))) 
        )
      (terpri)
      (when bits?
        (list-enabled-flags :frame (%aframe-info frame) " Frame flags: ")
        (list-enabled-flags :slot (%aframe-info frame) " Slot flags : ")
        )))
  (values))

(defun list-enabled-flags (flag-type mask header)
  (loop for flag in (get flag-type :flags) 
        with any-flag? = nil
        do
        (when (flagon? mask flag)
          (when (null any-flag?) (format t header))
          (format t "~S " flag)
          (setq any-flag? t)
          )
        finally (when any-flag?) (terpri)))


(defun temporary-frame? (f)
  (frame-flag-enabled? f :frame-temp?))

(defmethod describe-object ((frame %aframe) stream)
  (let ((*standard-output* stream)) (describe-frame frame)))

(defun df (frame &key (bits? t) (compute-slots? nil)) 
  "Describe (pretty print) a frame's slots and their values"
  (describe-frame frame :bits? bits? :compute-slots? compute-slots?))


(defun fpname (f)
  #.(one-string-nl
     "Returns pretty name (the name with the case as was originally typed in)"
     "if the pretty name exists, or the frame name (all lowercase).")
  (or (and *pretty-name-frame* (slotv f *pretty-name-frame*)) (fname f)))

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
           (multiple-value-bind (fname same?) 
               (%string->fname-same? new-name) 
             (setf (%aframe-fname frame) fname)
             (when (not same?)
               (setf (%slotv frame *pretty-name-frame*) new-name))
             (intern-frame frame fname)
             )))
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
      
(defun delete-frame (frame)
  #.(one-string-nl 
     '("Actually deletes a frame and removes the name from the frame namespace."
       "Must commit to really get rid of the acache object in a real acache."
       "WARNING: If there are any slots or other references to this frame, an "
       "acache error will be signaled in real acache each time anyone tries to "
       "access the frame (e.g., the slot). Therefore, this is likely to break everything!"
       ))
  (unintern-frame frame)
  (%delete-frame frame)
  t)

(defun delete-frame-contents (frame)
  "Removes all the slots of FRAME except for its FNAME."
  (purge-frame frame))

(defun frame-print-prefix (frame) (declare (ignore frame)) "#$")
  
(defun print-frame (frame stream &rest ignore)
  (declare (ignore ignore))
  (handler-case
      (format stream "~A~A" (frame-print-prefix frame) (fpname frame))
    (error () (format stream "##$~A" (%aframe-fname frame)))
    ))


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


(defvar *set-valued-non-list-action* :warn-and-convert)

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


#+test
(defun ct () 
  (let ((parent-frame (frames:frame-fnamed (string (gensym "PARENT-")) t))
        (frame) )
    (dotimes (n 3) 
      (setf frame (frames:frame-fnamed (format nil "test.~A" n) t)) 
      (setf (slotv parent-frame #$node)
            (cons frame (slotv parent-frame #$node)))
      ;; (push frame (#^node parent-frame)) 
      (format t "~%;; ITERATION ~D~%~%"(1+ n))
      (describe (%aframe-slots parent-frame))
      (format t "before : ~A~%" (#^node parent-frame)) 
      (format t ";; DOING COMMIT...")
      (db.ac:commit) 
      (describe (%aframe-slots parent-frame))
      (format t "after : ~A~%~%~%" (#^node parent-frame)) 
      )))

#+test
(defun ct1 () 
  (let ((parent-frame (frames:frame-fnamed (string (gensym "PARENT-")) t))
        frame
        (count 0))
    (dotimes (n 3) 
      (setf frame (frames:frame-fnamed (format nil "test.~A" n) t)) 
      (setf (slotv parent-frame #$node) (incf count))
      (format t "~%;; ITERATION ~D~%~%"(1+ n))
      (describe (%aframe-slots parent-frame))
      (format t "before : ~A~%" (#^node parent-frame)) 
      (format t ";; DOING COMMIT...")
      (db.ac:commit) 
      (describe (%aframe-slots parent-frame))
      (format t "after : ~A~%~%~%" (#^node parent-frame)) 
      )))
