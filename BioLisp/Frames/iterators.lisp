;; -*- Package: frames; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author:  Mike Travers.
;;; Modifications by JP Massar.


(defmacro with-new-frame-table ((frame-table-symbol) &body body)
  #.(one-string-nl
     "Execute BODY in the context of FRAME-TABLE-SYMBOL being bound to a new"
     "frame table object containing only the frame system frames, not any"
     "existing user frames.  Use this in conjuction with"
     "(WITH-ALTERNATIVE-FRAME-TABLE ...) to create an alternative universe"
     "of user frames and execute code in that context.")
  `(let ((,frame-table-symbol (copy-system-frame-table)))
     ,@body
     ))

(defmacro with-alternative-frame-table (frame-table &body body)
  #.(one-string-nl
     "Execute BODY in the dynamic context of an alternative frame table."
     "Use (WITH-NEW-FRAME-TABLE ...) to create the alternative table.")
  `(let ((*frame-table* ,frame-table))
     (unless (hash-table-p *frame-table*)
       (error "FRAME-TABLE must be a Lisp hash table: ~A" *frame-table*)
       (let ((*fname-frame* (gethash *frame-table* *fname-slot-name*)))
         (unless *fname-frame*
           (error "FRAME-TABLE ~A does not contain an FNAME (~A) entry!!"
                  *frame-table* *fname-slot-name*))
         ,@body
         ))))

(defmacro with-frames-iterated 
          ((symbol &optional (frame-table '*frame-table*)) &body body)
  #.(one-string-nl
     "Execute BODY once for each frame, with SYMBOL bound to that frame. "
     "By default, the iteration is over all frames in the "
     "universe, but if FRAME-TABLE is provided, the iteration is over the "
     "frames in FRAME-TABLE, which can be a hash table whose values are "
     "frames, or a sequence whose elements are frames.")
  (let ((fname (gensym "BODY-FUNCTION-"))
        (tblname (gensym "FTABLE-")))
    `(flet ((,fname (,symbol) ,@body))
       (let ((,tblname ,frame-table))
         (cond
          ((hash-table-p ,tblname)
           (maphash 
            (lambda (key value) (declare (ignore key)) (,fname value))
            ,tblname))
          ((typep ,tblname 'sequence) (map nil #',fname ,tblname))
          (t (error "Don't know how to iterate over ~A" ,tblname))
          )))))

(defmacro for-all-frames ((frame-symbol) &body body)
  #.(one-string-nl
     "Execute BODY once for each frame in the current frame table,"
     "with FRAME-SYMBOL bound to each frame.")
  `(maphash
    (lambda (key ,frame-symbol) (declare (ignore key)) ,@body)
    *frame-table*
    ))

(defun mapframes (function &optional (frame-table *frame-table*))
  #.(one-string-nl
     "Call FUNCTION on every frame and return a list of the results. "
     "By default, all frames in the universe are used, "
     "but if FRAME-TABLE is provided, only the frames in FRAME-TABLE, "
     "which can be a hash table whose values are "
     "frames, or a sequence whose elements are frames, are used.")
  (let ((results nil))
    (with-frames-iterated (f frame-table) (push (funcall function f) results))
    (nreverse results)))

(defun nmapframes (function &optional (frame-table *frame-table*))
  #.(one-string-nl
     "Call FUNCTION on every frame and return a list of the non-nil results. "
     "By default, all frames in the universe are used, "
     "but if FRAME-TABLE is provided, only the frames in FRAME-TABLE, "
     "which can be a hash table whose values are "
     "frames, or a sequence whose elements are frames, are used.")
  (let ((results nil))
    (with-frames-iterated (f frame-table) 
      (vwhen (value (funcall function f)) (push value results)))
    (nreverse results)))

(defun generic-frame-slots (frame) (%frame-slots frame))

;; Changed to make it identical to the sframes macro so that
;; compiled code can be shared between the two frame systems
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

;; This is a dummy for frames (as opposed to sframes), to make
;; the two systems compatible.  
(defmethod call-for-each-frame-slot-body-on-attached-slots 
           ((frame t) function values?)
  (declare (ignore function values?))
  nil
  )

(defun mapslots (function frame)
  #.(one-string-nl
     "Call FUNCTION (a function of two arguments, a FRAME-SLOT and a VALUE)"
     "on every SLOT / VALUE pair of FRAME an return a list of results.")
  (for-each-frame-slot (slot-frame value) frame
    (funcall function slot-frame value)
    ))
