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

;;; Authors:  Mike Travers, JP Massar.

;;; Simple frame system.
;;; Mike Travers, April 2003.
;;; JP Massar, August 2004, modifications for serialization, persistence, etc.

#|

SHORT FRAMES DESCRIPTION

Frames have a unique FNAME.  This is used to refer to them from code,
Lisp listeners, persistent DBs, and UIs.  When frames refer to each other 
in memory, they use pointers. FNAMEs have no inherent semantic meaning.

FNAMEs are strings, internally. They are case-preserving and case-insensitive
(like Windows filenames as opposed to Unix filenames).  The allowed chars
are any except whitespace, (, ). 

Frames can be written out in code or elsewhere as #$Hexokinase, where 
"Hexokinase" is the fname of the frame representing you-know-what.

Access to the slot value of a frame can be written as

(#^slot-name frame), or using SLOTV:  (slotv frame slot-name)

So (slotv #$Hexokinase #$Goid) and (#^Goid #$Hexokinase) are equivalent.

|#


;;;; IMPLEMENTATION OF FRAME DATA STRUCTURE AND LOWEST LEVEL PRIMITIVES

#+:SBCL
;; This will produce a redefinition warning in SBCL, but otherwise
;; it complains about the function not being defined because of its
;; use in the DEFSTRUCT below.  (But it shouldn't because the compilation
;; is within the scope of a WITH-COMPILATION-UNIT).
(defun print-frame (frame stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#$~A" (%slotv frame (frame-fnamed "fName"))))

(defstruct (%frame (:print-function print-frame))
  (slots))

(defun frame-print-prefix (frame)
  (declare (ignore frame))
  "#$")

;;; Primitive slot access
(defun %slotv (frame slot)
  #.(optimization-declaration)
  (declare (type %frame frame))
  (loop for slot-value-dotted-pair in (%frame-slots frame) do
        (when (eq (car slot-value-dotted-pair) slot)
          (return (cdr slot-value-dotted-pair))
          )))
  
;;; Primitive slot setting
(defun %set-slotv (frame slot value)
  #.(optimization-declaration)
  (declare (type %frame frame))
  (block exit
    (loop for slot-value-dotted-pair in (%frame-slots frame) do
          (when (eq (car slot-value-dotted-pair) slot)
            (return-from exit (setf (cdr slot-value-dotted-pair) value))
            ))
    (push (cons slot value) (%frame-slots frame))
    value
    ))

;;; This is the bit of magic that makes SETF, PUSH and their ilk work
;;; with the %slotv accessor function.
(defsetf %slotv %set-slotv)

(defun %remove-slot (frame slot)
  (setf (%frame-slots frame) (delete slot (%frame-slots frame) :key 'car))
  frame)
  


;;;; BASICS FOR FRAMES TABLE


;;; By default create a small frame table.  Application initialization code
;;; can call RESIZE-FRAME-TABLE as appropriate before creating shitloads
;;; of frames.

(defparameter *default-frame-table-size* 800)

;;; The actual frames table will be created in bootstrap.lisp

(defvar *frame-table* nil)

(defparameter *fname-slot-name* "fName")

(defvar *fname-frame* nil)

;;; use an equalp hash table for case-insensitivity.

(defun create-a-frame-table (&optional (size *default-frame-table-size*))
  (make-string-equal-hash-table :size size)
  )

;;; BASICS FOR FRAMES READER

(defparameter *illegal-frame-chars*
  (coerce 
   (append (list (schar "(" 0) (schar ")" 0)) *whitespace*) 
   'simple-string)
  "Characters that are not allowed in strings representing frame names")

(defun valid-frame-char? (x) (null (find x *illegal-frame-chars*)))


;;; Define a MAKE-LOAD-FORM method for a frame.  a MAKE-LOAD-FORM method
;;; is supposed to return a form such that when it is evaluated, the
;;; original object (in some sense) is recreated.  This is used by the
;;; compiler to store certain objects in .fasl files, insuring they get
;;; recreated properly at fasl load time.

(defvar cl-user::*frames-package* :frames)

(defun recreate-frame (name)
  (if (find-package :aframes)
      (ecase cl-user::*frames-package*
        (:aframes
         (forward-package-funcall :aframes :frame-fnamed name t))
        (:frames (frame-fnamed name t)))
    (frame-fnamed name t)
    ))

(defmethod make-load-form ((frame %frame) &optional ignore)
  (declare (ignore ignore))
  (let ((fname (%slotv frame (frame-fnamed *fname-slot-name*))))
    `(recreate-frame ,fname)
    ))

;;; The frames created by the frames system itself.

(defvar *frames-system-frames* nil)




