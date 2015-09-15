; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author:  Mike Travers.
;;; Modifications by JP Massar.

(defun frame-has-slot? (frame slot)
  #.(one-string-nl
     "Does FRAME actually have a slot SLOT?"
     "(note that (SLOTV FRAME SLOT) will return NIL if the slot does not"
     "exist OR if the slot value is NIL.")
  (block exit
    (for-each-frame-slot (sf v) frame
      ;; Shut the compiler up
      (progn v (when (eq slot sf) (return-from exit t))))))

(defmethod frame-slots-of ((frame frame))
  "A list of frames which are the slots of FRAME."
  (let ((frame-slots nil))
    (for-each-frame-slot (sf v) frame
      (progn v (push sf frame-slots)))
    frame-slots
    ))

(defmacro fff (string)
  "Returns the frame named STRING or if none exists creates it."
  `(frame-fnamed ,string t))

(defun resize-frame-table (size)
  (declare (ignore size))
  ;; no op for now
  nil
  )

