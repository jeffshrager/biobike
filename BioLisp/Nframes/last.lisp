;; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

(defvar *system-frames-initialized?* nil)

(progn 
  (cond 
   (cl-user::*acache-running* 
    (setq *frames-system-frames* 
          (cons #$fname (mapframes 'identity :framespace :sys))))
   ((null *system-frames-initialized?*)
    (let ((all-frames (mapframes 'identity))
          (fsf (cons #$fname (mapframes 'identity :framespace :sys))))
      (unless (= (length all-frames) (length fsf))
        (cformatt "Frames exist at this point which are not system frames!!")
        (cformatt "These frames are: ~A" (set-difference all-frames fsf))
        (error "Fix this"))
      (setq *frames-system-frames* fsf)
      ))
   (t 
    (cformatt "*** System frames not being reinitialized.")
    (cformatt "*** Evaluate (setq frames::*system-frames-initialized?* nil)")
    (cformatt "*** to force reinitialization.")
    (cformatt "*** Note that forcing this reinitialization will cause all")
    (cformatt "*** existing frames to be marked as system frames, which is")
    (cformatt "*** probably not what you want.")
    ))
  (setq *system-frames-initialized?* t)
  )
        
(defun safe-slotv (frame slot)
  (let ((value (slotv frame slot)))
    (or value
        (unless (or (frame-has-slot? frame slot) *suppress-warnings*)
          (warn 
           #.(one-string-nl
              "Slot ~S of frame ~A does not exist. NIL returned."
              "To suppress this warning use (assign *suppress-warnings* t)")
           slot frame)
          value
          ))))

(define-ref-methods nil ((obj frame::%aframe) slot &rest slots)
  (if (null slots)
      (if (consp slot)
          (loop for s in slot collect (safe-slotv obj s))
        (safe-slotv obj slot))
    (mapcar (lambda (s) (safe-slotv obj s)) (cons slot slots))
    ))

(define-ref-methods t ((new-value t) (obj frames::%aframe) slot &rest slots)
  (flet ((setf-fmap (slots)
           (loop for aslot in slots 
                 for j fixnum from 0
                 do
                 (setf (slotv obj aslot) (elt new-value j))
                 )))
    (if (null slots)
        (if (consp slot)
            (setf-fmap slot)
          (setf (slotv obj slot) new-value))
      (setf-fmap (cons slot slots))
      ))
  new-value)


(defstruct (frame-iterator (:include iterator)))

(defmethod iter-init ((obj frames::%aframe))
  (make-frame-iterator 
   :obj obj
   :state (let ((slot-list nil))
            (for-each-frame-slot (slot value) obj
                                 (push (list slot value) slot-list))
            (setq slot-list (nreverse slot-list))
            )))

(defmethod iter-next? ((i frame-iterator)) (utils::iterator-state i))
(defmethod iter-next ((i frame-iterator))
  (pop (utils::iterator-state i)))
  
  

(defmethod garray-convert-hash-index
           ((hash-index frames::%aframe) (hash-table-test (eql 'equalp)))
  (fname hash-index))


