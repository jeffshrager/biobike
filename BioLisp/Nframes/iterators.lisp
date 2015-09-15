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

(defmacro with-new-frame-table ((frame-table-symbol) &body body)
  #.(one-string-nl
     "Execute BODY in the context of FRAME-TABLE-SYMBOL being bound to a new"
     "frame table object containing only the frame system frames, not any"
     "existing user frames.  Use this in conjuction with"
     "(WITH-ALTERNATIVE-FRAME-TABLE ...) to create an alternative universe"
     "of user frames and execute code in that context.")
  (declare (ignore frame-table-symbol body))
  `(error "This doesn't work anymore."))

#+does-not-work
(defmacro with-alternative-frame-table (frame-table &body body)
  #.(one-string-nl
     "Execute BODY in the dynamic context of an alternative frame table."
     "Use (WITH-NEW-FRAME-TABLE ...) to create the alternative table.")
  `(let ((*framespace-table* ,frame-table)
         (*fname-frame* *fname-frame*)
         (*pretty-name-frame* *pretty-name-frame*))
     (cond 
      ((or (typep *framespace-table* 'ac-map)  (hash-table-p *frame-table*))
       (setq *fname-frame*
             (%string->frame 
              (default-framespace *framespace-table*) *fname-slot-name*))
       (setq *pretty-name-frame* 
             (%string->frame 
              (default-framespace *framespace-table*) *pretty-name-frame-name*))
       (unless (and *fname-frame* *pretty-name-frame*)
         (error "FRAME-TABLE ~A does not contain an FNAME (~A) entry!!"
                *frame-table* *fname-slot-name*)))
      (t (error "Illegal object to use as frame table: ~A" *framespace-table*)))
     ,@body
     ))

(defmacro with-frames-iterated 
          ((symbol 
            &optional (framespace nil) 
            &key (framespace-table '*framespace-table*))
           &body body)
  #.(one-string-nl
     "Execute BODY once for each frame, with SYMBOL bound to that frame. "
     "By default, the iteration is over all frames in the current frame"
     "world, but if FRAMESPACE is non-nil and provided, the iteration is"
     "over the frames in FRAMESPACE which can be a hash table whose values are"
     "frames, or a sequence whose elements are frames.  In this case,"
     "FRAMESPACE-TABLE is ignored."
     "If FRAMESPACE is either nil or not provided, then the iteration"
     "is over all frames in the value of FRAMESPACE-TABLE, which defaults"
     "to the current frame world.")
  (let ((body-function (gensym "BODY-FUNCTION-"))
        (fsname (gensym "FS-")))
    `(flet ((,body-function (,symbol) ,@body))
       (let ((,fsname ,framespace))
         (cond
          ((null ,fsname)
           (iterate-over-all-frames-in-world 
            #',body-function ,framespace-table))
          (t 
           (iterate-over-framespace
            #',body-function (to-framespace ,fsname t))
           ))))))

(defun iterate-over-all-frames-in-world (body-function world)
  (let ((*framespace-table* world))
    (cond 
     ((typep world 'ac-map)
      (map-map 
       (lambda (key framespace) 
         (unless (keywordp key)
           (iterate-over-framespace body-function framespace)))
       world))
     ((hash-table-p world)
      (maphash 
        (lambda (key framespace)
          (unless (keywordp key)
            (iterate-over-framespace body-function framespace)))
        world))
     (t (error "Unknown framespace type: ~A" world))
     )))

(defun iterate-over-framespace (body-function framespace)
  (cond
   ((typep framespace 'ac-map)
    (map-map 
     (lambda (key value) 
       (unless (keywordp key) (funcall body-function value)))
     framespace)) 
   ((hash-table-p framespace)
    (maphash 
     (lambda (key value) 
       (unless (keywordp key) (funcall body-function value)))
     framespace))
   ((typep framespace 'sequence) (map nil body-function framespace))
   (t (error "Don't know how to iterate over ~A" framespace))
   ))

(defmacro for-all-frames ((frame-symbol &optional (framespace nil)) &body body)
  #.(one-string-nl
     "Execute BODY once for each frame in the current frame world,"
     "with FRAME-SYMBOL bound to each frame.")
  `(with-frames-iterated (,frame-symbol ,framespace) ,@body))

(defun mapframes (function &key (framespace nil) (verbose? t))
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
    (when (null framespace) 
      (when verbose? 
        (cformatt "You are iterating over every frame in the world!")
        (cformatt 
         "You could specify a framespace name to limit the iteration.")))
    (with-frames-iterated (f framespace) (push (funcall function f) results))
    (nreverse results)))

(defun nmapframes (function &key (framespace nil) (verbose? t))
  #.(one-string-nl
     "Call FUNCTION (a function of one argument, a frame) on every"
     "frame and return a list of the non-nil results. "
     "See (mapframes ...) for details.")
  (let ((results nil))
    (when (null framespace) 
      (when verbose? 
        (cformatt "You are iterating over every frame in the world!")
        (cformatt 
         "You could specify a framespace name to limit the iteration.")))
    (with-frames-iterated (f framespace) 
      (vwhen (value (funcall function f)) (push value results)))
    (nreverse results)))

(defmacro for-each-frame-slot 
          ((slot-symbol value-symbol) frame &body body)
  #.(one-string-nl
     "Loop over every slot of FRAME, executing BODY with SLOT-SYMBOL"
     "bound to the frame naming the slot, and VALUE-SYMBOL bound to"
     "the value of the slot.")
  (let ((body-function (gensym "FRAME-ITERATOR-")))
    `(flet ((,body-function (,slot-symbol ,value-symbol) ,@body))
       (slot-iterator ,frame (function ,body-function))
       )))

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
