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

;;; Author: JP Massar.


(defmacro def-aframe-type (type (&rest supertypes))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defaclass ,type ,supertypes ())
     (let ((new-type-frame (frame-fnamed (string ',type) t)))
       (loop for supertype in ',supertypes
             as sframe = (frame-fnamed (string supertype) t)
             do
             (pushnew new-type-frame (slotv sframe #$subtypes))
             (pushnew sframe (slotv new-type-frame #$supertypes))
             ))))

#||

;;; Returns two values.  If the slot is found, returns the value
;;; of the slot, and T.  If the slot is not found, returns NIL and NIL.
(defun generate-%fixed-slotv-method (type instance-slot-descriptors)
  `(defmethod %fixed-slotv ((frame ,type) slot)
     (declare (ignorable slot))
     (cond
      ,@(mapcar
        (lambda (isd) 
          `((eq slot ,(fsdesc-slot isd)) 
            (values (,(fsdesc-accessor isd) frame) t)))
        instance-slot-descriptors
        )
      (t (call-next-method))
      )))

(defmethod %fixed-slotv ((frame %typed-frame) slot)
  (declare (ignore slot))
  (values nil nil))

;;; Returns T if slot is found and set, NIL if not found.
(defun generate-%fixed-set-slotv-method (type instance-slot-descriptors)
  `(defmethod %fixed-set-slotv ((frame ,type) slot value)
     (declare (ignorable slot value))
     (cond
      ,@(mapcar
        (lambda (isd)
          `((eq ,(fsdesc-slot isd) slot) 
            (setf (,(fsdesc-accessor isd) frame) value) t))
        instance-slot-descriptors
        )
      (t (call-next-method))
      )))

(defmethod %fixed-set-slotv ((frame %typed-frame) slot value)
  (declare (ignore slot value))
  nil)
  

;;; Returns NIL if SLOT is not a fixed slot of FRAME.
(defun generate-%fixed-slot?-method (type instance-slot-descriptors)
  `(defmethod %fixed-slot? ((frame ,type) slot)
     (or (member slot ',(mapcar 'fsdesc-slot instance-slot-descriptors)) 
         (call-next-method))
     ))

(defmethod %fixed-slot? ((frame %typed-frame) slot)
  (declare (ignore slot)) 
  nil)
              

(defun generate-slot-iterator-method (type instance-slot-descriptors)
  `(defmethod slot-iterator ((frame ,type) function)
     (declare (ignorable function))
     ,@(loop for isd in instance-slot-descriptors
             as fs = (fsdesc-slot isd)
             as fa = (fsdesc-accessor isd)
             collect
             `(funcall function ,fs (,fa frame)))
     (call-next-method)
     ))

(defun generate-typed-instance-initialization-method
       (type frame-class instance-slot-descriptors)
  `(defmethod %initialize-frame-instance ((frame ,type))
     (call-next-method)
     (setf (%typed-frame-frame-class frame) ,frame-class)
     ,@(loop for fs in instance-slot-descriptors collect
             `(setf (,(fsdesc-accessor fs) frame) ,(fsdesc-initform fs)))))

(defmethod %initialize-frame-instance ((frame %typed-frame))
  (enable-frame-flag frame :frame-typed?))


(defmethod %remove-slot ((frame %typed-frame) slot)
  (when (%fixed-slot? frame slot)
    (error "You cannot remove slot ~A from frame ~A. It is a fixed slot!"
           frame slot))
  (call-next-method))

(defmethod %non-fixed-set-slotv ((frame %typed-frame) slot value)
  (%set-slotv-no-frame-flags frame slot value))

;;; For typed-frames we do not eliminate the values of the fixed slots,
;;; by definition.  Therefore the generic method will work.  This is here
;;; simply to make this explicit.  
(defmethod %purge-frame ((frame %typed-frame) ignore reinitialize-fixed-slots?)
  (declare (ignore ignore))
  (call-next-method) 
  (when reinitialize-fixed-slots? (%initialize-frame-instance frame)))


(defmethod %inherited-slotv ((frame %typed-frame) slot)
  (if (or (eq slot *pretty-name-frame*) (gethash slot *system-flag-frames*))
      (values nil nil)
    (let ((frame-class-frame (%typed-frame-frame-class frame)))
      (cond
       ((null frame-class-frame) 
        (error "Internal error: Typed frame without frame class!"))
       ((listp frame-class-frame)
        (error "Internal error: Frame class should not be a list!"))
       (t
        (let ((inheritance-list (build-inheritance-list frame-class-frame)))
          (loop for super in inheritance-list do
                (multiple-value-bind (rval found?)
                    (slotv super slot)
                  (when found? (return (values rval found?))))
                finally (return (values nil nil))
                )))))))

;;; Creates depth-first, not breath-first.  Fix.

(defun build-inheritance-list (frame-class-frame)
  (labels ((do-it (fcf)
             (let ((class-slots (slotv fcf #$sys.inheriting-frame-classes)))
               (if (null class-slots)
                   (list fcf)
                 (cons fcf (mapcan #'do-it class-slots))
                 ))))
    (let ((result (do-it frame-class-frame)))
      (if (= 1 (length result))
          result
        (purge-duplicates (do-it frame-class-frame))
        ))))
      

(defmethod wob-html :after ((frame %typed-frame))
  (let ((*current-object* frame)
        (class-frame 
         (frame-class-frame (type-of frame) :if-does-not-exist nil)))
    (declare (special *current-object*))
    (flet ((title (s)
             (html :p (:b (:i (:princ-safe s))) 
                   " ( " (forward-funcall 'emit-value class-frame) ")"
                   :br)))
      (when class-frame
        (title "Frame class hierarchy")
        (forward-funcall
         'emit-ancestor-frames 
         class-frame 
         (lambda (x) (#^sys.inheriting-frame-classes x))
         (lambda (x) (#^sys.frame-subclasses x))
         )))))

||#