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

(defvar *temp-frame-counter-frame* #$temp-frame-counter)

;;; Call this to make a temp frame
;;; This uses the frame type 'temp-frame to establish a class of temporary frames
;;; which we can then delete at system startup in boot-frame-system by 
;;; calling delete-all-temporary-frames.  
(defun make-temp-frame (&optional class)
  (vif (count (slotv #$temp-frame-counter #$counter))
       (prog1
           (let ((tframe (make-frame-instance
                     'temp-frame (formatn "TEMP-FRAME-~D" count) t)))
             (when class (setf (#^instanceof tframe) class))
             tframe
             )
         (incf (slotv #$temp-frame-counter #$counter)))
       (progn
         (setf (slotv #$temp-frame-counter #$counter) 1)
         (make-temp-frame)
         )))

(defun delete-temp-frame (frame)
  (declare (ignore frame))
  nil)


