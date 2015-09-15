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

(defun safe-slotv (frame slot)
  (multiple-value-bind (value slot-exists?)  
      (slotv frame slot)
    (or value
        (unless (or slot-exists? *suppress-warnings*)
          (warn 
           #.(one-string-nl
              "Slot ~S of frame ~A does not exist. NIL returned."
              "To suppress this warning use (assign *suppress-warnings* t)")
           slot frame)
          value
          ))))

(define-ref-methods nil ((obj aframe) slot &rest slots)
  (if (null slots)
      (if (consp slot)
          (loop for s in slot collect (safe-slotv obj s))
        (safe-slotv obj slot))
    (mapcar (lambda (s) (safe-slotv obj s)) (cons slot slots))
    ))

(define-ref-methods t ((new-value t) (obj aframe) slot &rest slots)
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

(defmethod iter-init ((obj aframe))
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
           ((hash-index aframe) (hash-table-test (eql 'equalp)))
  (fname hash-index))


