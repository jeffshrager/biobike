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

(def-aframe-type structure-indirector (aframe))

(defun attached-frame-name (frame slot) (s+ (fname frame) "@" (fname slot)))

(defun attached-frame (frame slot &optional (force? nil))
  (frame-fnamed 
   (attached-frame-name frame slot) force? 'structure-indirector))

(defun maybe-create-and-set-attached-frame (frame slot &optional (data nil))
  (let ((attached-frame (attached-frame frame slot t)))
    (setf (slotv attached-frame #$data) data)
    attached-frame
    ))

(defun attached-frame-value (frame slot &key (if-not-attached? :error))
  (let ((container-frame (attached-frame frame slot)))
    (if (null container-frame)
        (case if-not-attached?
          (:error 
           (error "No attached frame for slot ~A of frame ~A" slot frame))
          (:warn
           (warn "No attached frame for slot ~A of frame ~A" slot frame))
          (otherwise if-not-attached?)
          )
      (slotv container-frame #$data)
      )))