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


;;; Call this to make a temp frame
(defun make-temp-frame (&optional class)
  #.(one-string-nl
     "Create a frame which is not interned.  It is not put in the table "
     "of all accessible frames. The class is forced into the #$instanceOf slot, which"
     "may or may not be used downstream by magical realism.")
  (let ((frame (%make-temp-frame)))
    (when class (setf (slotv frame #$sys.instanceOf) class))
    frame
    ))

(defun temp-frame-name ()
  (let ((counter-value (slotv #$sys.temp-frame-specifier #$sys.counter)))
    (when (null counter-value)
      (setf (slotv #$sys.temp-frame-specifier #$sys.counter) 0)
      (setf counter-value 0))
    (let ((counter (1+ counter-value)))
      (setf (slotv #$sys.temp-frame-specifier #$sys.counter) counter)
      (formatn "temp.t~D" counter)
      )))
    
(defun delete-temp-frame (frame)
  (%delete-frame frame))



