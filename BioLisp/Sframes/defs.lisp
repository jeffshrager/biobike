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

(defmacro defaclass (name superclasses slot-descriptors)
  #.(one-string-nl
     "Creates or redefines a class to be either persistent or standard-class"
     "depending on whether Allegrocache is running") 
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if user::*acache-running*
         (funcall
          (compile
           nil
           (eval
            '(lambda ()
               (defclass ,name ,superclasses 
                 ,slot-descriptors
                 (:metaclass db.ac:persistent-class)
                 )))))
       (funcall
        (compile
         nil
         (lambda ()
           (defclass ,name ,superclasses 
             ,slot-descriptors
             )))))))


(defparameter *fname-slot-name* "fName")

(defvar *fname-frame* nil)

(defparameter *iname-slot-name* "iName")

(defvar *iname-frame* nil)

(defvar *after-frame-creation-hook* nil)

#||

(defvar *timestamp-frame-name* "sys.timestamp")

(defvar *timestamp-frame* nil)


(defvar *after-frame-creation-hook* nil)
(defvar *after-frame-modification-hook* nil)

(defbit :frame :frame-access 0)
(defbit :frame :frame-permissions 1)
(defbit :frame :frame-autoload? 2)
(defbit :frame :frame-temp? 3)
(defbit :frame :frame-typed? 4)
(defbit :frame :frame-modified? 5)
(defbit :frame :frame-permanent? 6)

(defbit :slot :slot-access 15)
(defbit :slot :slot-change 16)
(defbit :slot :slot-domain 17)
(defbit :slot :slot-domain-list? 18)
(defbit :slot :slot-domain-set? 19)
(defbit :slot :slot-computed 20)
(defbit :slot :slot-always-computed 21)
(defbit :slot :slot-inherited 22)

(defbit :fslot :fslot-domain 0)
(defbit :fslot :fslot-permissions 1)
(defbit :fslot :fslot-change 2)
(defbit :fslot :fslot-no-slot-check? 3)
(defbit :fslot :fslot-modified? 4)
(defbit :fslot :fslot-domain-list? 16)
(defbit :fslot :fslot-domain-set? 17)

||#

;;;; IMPLEMENTATION OF FRAME DATA STRUCTURE AND LOWEST LEVEL PRIMITIVES

(defaclass %frame (frame) ())

(defaclass 
 aframe (%frame)
 ((fname :initarg :fname :accessor aframe-fname)
  (iname :initarg :iname :accessor aframe-iname :index :any-unique)
  ;; (info :initarg :info :accessor aframe-info :initform 0)
  (slots :initarg :slots :accessor aframe-slots :initform nil))
 )

(defaclass temp-frame (aframe) ())

;; (defaclass simple-frame (aframe) ())

;;; Primitive slot access

(defmethod %slotv ((frame frame) slot) 
  (declare (ignore slot))
  (error "Internal error!  Should not get here!"))

(defmethod %slotv ((frame aframe) slot)
  #.(optimization-declaration)
  (loop for (fslot . value) in (aframe-slots frame)
        do
        (when (eq slot fslot) (return (values value t)))
        finally (return (values nil nil))
        ))

(defmethod %set-slotv ((frame aframe) slot value)
  #.(optimization-declaration)
  (loop for data in (aframe-slots frame) 
        as fslot = (car data)
        do
        (when (eq fslot slot)
          (setf (cdr data) value)
          (db.ac:mark-instance-modified frame)
          (return (values value :existing)))
        finally 
        (progn
          (push (cons slot value) (aframe-slots frame))
          (db.ac:mark-instance-modified frame)
          (return (values value :new))
          )))

(defsetf %slotv %set-slotv)

;;; Returns T if slot actually removed, otherwise NIL.
(defmethod %remove-slot ((frame aframe) slot)
  (and (find slot (aframe-slots frame) :test 'eq :key 'car)
       (progn
         (setf (aframe-slots frame)
               (delete slot (aframe-slots frame) :test 'eq :key 'car))
         (db.ac:mark-instance-modified frame)
         t 
         )))
    
(defun %delete-frame (frame) (delete-persistent-object frame))

(defun delete-persistent-object (x)
  (when user::*acache-running* (db.ac:delete-instance x)))
  
(defgeneric slot-iterator (frame function)
  (:documentation
   #.(one-string-nl 
      "Calls FUNCTION, a function of 2 arguments (a slot frame and a"
      "slot value), on each of the slot/value pairs of FRAME, including"
      "the fname slot.")))

(defmethod slot-iterator ((frame aframe) function)
  #.(optimization-declaration)
  (funcall function *fname-frame* (aframe-fname frame))
  (funcall function *iname-frame* (aframe-iname frame))
  (loop for (fslot . value) in (aframe-slots frame)
        do
        (funcall function fslot value)
        ))

(defun framep (x) 
  "Whether the object is a frame.  A predicate returning T or NIL."
 (typep x 'frame))

(defun isframe? (x) 
  "Whether the object is a frame.  A predicate returning T or NIL."
  (typep x 'frame))

(defun fname (frame) (aframe-fname frame))

(defun iname (frame) (aframe-iname frame))

(defun fstring (x) 
  "Generalization of STRING function to include FRAME objects"
  (if (framep x) (fname x) (string x)))

;;; Define a MAKE-LOAD-FORM method for a frame.  a MAKE-LOAD-FORM method
;;; is supposed to return a form such that when it is evaluated, the
;;; original object (in some sense) is recreated.  This is used by the
;;; compiler to store certain objects in .fasl files, insuring they get
;;; recreated properly at fasl load time.

(defvar cl-user::*frames-package*)
(setq cl-user::*frames-package* :aframes)

(defun recreate-frame (name)
  (if (find-package :aframes)
      (frame-fnamed name t)
    (error "Package aframes does not exist!")))

(defmethod make-load-form ((frame aframe) &optional ignore)
  (declare (ignore ignore))
  (let ((fname (aframe-fname frame)))
    `(recreate-frame ,fname)
    ))

(defmethod print-object ((frame aframe) stream)
  (if (fboundp 'print-frame)
      (print-frame frame stream)
    (format stream "#$~A" (aframe-fname frame))))

(defmethod framedbg ((f aframe))
  (terpri)
  (formatt "Fname: ~S~%" (aframe-fname f))
  (formatt "Iname: ~S~%" (aframe-iname f))
  (formatt "~D Slots: ~%" (length (aframe-slots f)))
  (loop for (fslot . value) in (aframe-slots f)
        do
        (formatt "  #$~S : ~S~%" (aframe-fname fslot) value)
        )
  (terpri)
  )

