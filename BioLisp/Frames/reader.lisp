;;; -*- Package: frames; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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


(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; This MUST be a DEFVAR, not a DEFPARAMETER!
  ;; Otherwise upon reloading this file the system would get
  ;; the frames-modified readtable...

  (defvar *saved-standard-readtable* (copy-readtable))

  (defun read-fname (stream)
    (read-until 
     stream
     (lambda (char)
       (or (member char *whitespace*)
           (member char '(#\( #\)))))
     (new-string)
     t))

  ;;; Support for #$ and #^ syntax
  (defun pound-dollar-frame-reader (stream char arg)
    (declare (ignore char arg))
    (frame-fnamed (read-fname stream) t))

  (defun pound-carat-frame-reader (stream char arg)
    (declare (ignore char arg))
    (let* ((slot-frame (frame-fnamed (read-fname stream) t)))
      `(lambda (x) (slotv x ,slot-frame))
      ))

  (set-dispatch-macro-character #\# #\$ 'pound-dollar-frame-reader)
  (set-dispatch-macro-character #\# #\^ 'pound-carat-frame-reader)

  ;;; This must be AFTER the definition of any dispatch macro
  ;;; characters, so that the weblistener will get a correct copy
  ;;; of the readtable.

  (defparameter *frame-modified-readtable* (copy-readtable))



  )

(defun saved-standard-readtable () 
  #.(one-string-nl
     "Returns the STANDARD READTABLE (or at least, the value of *READTABLE*"
     "before the frames system gets loaded.")
  *saved-standard-readtable*)

(defun frames-readtable () 
  #.(one-string-nl
     "Returns a READTABLE which has been modified by the frame system"
     "to handle #$ and #^ syntax.")
  *frame-modified-readtable*)

;;; Must be in a different EVAL-WHEN since it uses #$ which is
;;; defined above.  If in a single EVAL-WHEN the reader would try
;;; to interpret #$ before its definition had been executed.



(defmacro framedcl (&rest frames)
  (declare (ignore frames))
  (warn "Rejoice, rejoice, FRAMEDCL is obsolete!!!!"))

;;; Permits fi:lisp emacs mode to send over #$ etc syntax.
;;; Need to have a mode line, like: 
    ;; -*- mode: common-lisp; package: user; readtable: blrt -*-
;;; in the top of your file, AND to have loaded the file with
;;; this mode line into Emacs (i.e., visited it with the mode line)

#+:Allegro
(setf (excl::named-readtable :blrt) 
      (frames-readtable))

