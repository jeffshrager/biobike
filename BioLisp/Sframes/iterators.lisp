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

;;; Author: JP Massar

(defmacro with-frames-iterated 
          ((symbol &optional (frame-type ''aframe)) &body body)
  #.(one-string-nl
     "Execute BODY once for each frame, with SYMBOL bound to that frame. "
     "If FRAME-TYPE is a subclass of frames, then only those frame of that"
     "subclass will be iterated over.")
  `(db.ac::doclass (,symbol ,frame-type) ,@body)
  )

(defmacro for-all-frames 
          ((frame-symbol &optional (frame-type 'aframe)) &body body)
  #.(one-string-nl
     "Execute BODY once for each frame,"
     "with FRAME-SYMBOL bound to each frame."
     "If FRAME-TYPE is a subclass of frames, then only those frame of that"
     "subclass will be iterated over.")
  `(with-frames-iterated (,frame-symbol ,frame-type) ,@body))


(defun mapframes (function &key (frame-type 'aframe) (verbose? t))
  #.(one-string-nl
     "Call FUNCTION (a function of one argument, a frame) on every frame"
     "in FRAMESPACE and return a list"
     "of the results.  If FRAMESPACE is nil, all frames in the current"
     "frame world are used.  If FRAMESPACE is a string, its name is used"
     "to find an actual framespace in the current frame world.  If FRAMESPACE"
     "is a non-nil sequence the elements are processed as if by LISP:MAP."
     "Otherwise FRAMESPACE must be a real framespace object and if so"
     "the frames contained in it are used.")
  (let ((results nil))
    (when (eq frame-type 'aframe)
      (when verbose? 
        (cformatt "You are iterating over every frame in the world!")
        (cformatt "Specify a frame subtype to limit the iteration.")))
    (with-frames-iterated (f frame-type) (push (funcall function f) results))
    (nreverse results)))

(defun nmapframes (function &key (frame-type 'aframe) (verbose? t))
  #.(one-string-nl
     "Call FUNCTION (a function of one argument, a frame) on every"
     "frame and return a list of the non-nil results. "
     "See (mapframes ...) for details.")
  (let ((results nil))
    (when (eq frame-type 'aframe) 
      (when verbose? 
        (cformatt "You are iterating over every frame in the world!")
        (cformatt "Specify a frame subtype to limit the iteration.")
        ))
    (with-frames-iterated 
     (f frame-type) 
     (vwhen (value (funcall function f)) (push value results)))
    (nreverse results)))

(defun generic-frame-slots (frame) (aframe-slots frame))

;; Modified to make it exactly like the one in Frames/iterators.lisp
;; so code can be shared.  
(defmacro for-each-frame-slot 
          ((slot-symbol &optional (value-symbol nil)) frame &body body)
  #.(one-string-nl
     "Loop over every slot of FRAME, executing BODY with SLOT-SYMBOL"
     "bound to the frame naming the slot, and if VALUE-SYMBOL"
     "is non-nil, WITH VALUE-SYMBOL bound to the value of the slot.")
  (let ((body-function (gensym "FRAME-ITERATOR-"))
        (frame-symbol (gensym "FRAME-"))
        (cons-symbol (gensym "CONS-")))
    `(flet ((,body-function 
                    (,slot-symbol 
                     ,@(when value-symbol `(&optional (,value-symbol nil))))
              (declare 
               (ignorable 
                ,slot-symbol
                ,@(and value-symbol (list value-symbol))
                ))
              ,@body))
       (let ((,frame-symbol ,frame))
         (,body-function 
          *fname-frame*
          ,@(when value-symbol `((fname ,frame-symbol))))
         ,(if value-symbol
              `(loop for (,slot-symbol . ,value-symbol) in 
                     (generic-frame-slots ,frame-symbol)
                     unless (eq ,slot-symbol *fname-frame*)
                     do
                     (,body-function ,slot-symbol ,value-symbol))
            `(loop for ,cons-symbol in 
                   (generic-frame-slots ,frame-symbol) 
                   as ,slot-symbol = (first ,cons-symbol)
                   unless (eq ,slot-symbol *fname-frame*)
                   do
                   (progn ,value-symbol (,body-function ,slot-symbol))
                   ))
         (call-for-each-frame-slot-body-on-attached-slots
          ,frame-symbol #',body-function ,(not (null value-symbol))
          )))))

(defmethod call-for-each-frame-slot-body-on-attached-slots 
           ((frame aframe) function call-with-slot-value?)
  (declare (ignore function call-with-slot-value?))
  nil
  )

(defun mapslots (function frame)
  #.(one-string-nl
     "Call FUNCTION (a function of two arguments, a FRAME-SLOT and a VALUE)"
     "on every SLOT / VALUE pair of FRAME an return a list of results.")
  (for-each-frame-slot (slot-frame value) frame
    (funcall function slot-frame value)
    ))

(defun slots-and-values (frame)
  "Returns a list of all (slot value) pairs in FRAME."
  (let ((result nil)) 
    (mapslots (lambda (slot value) (push (list slot value) result)) frame)
    result))
