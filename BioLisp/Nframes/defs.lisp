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

;;; Author: JP Massar.

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
                 (:metaclass persistent-class)
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

(defparameter *pretty-name-frame-name* "sys.pretty-name")

(defvar *pretty-name-frame* nil)

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

;;;; IMPLEMENTATION OF FRAME DATA STRUCTURE AND LOWEST LEVEL PRIMITIVES


(defaclass 
 %aframe ()
 ((fname :initarg :fname :accessor %aframe-fname)
  (info :initarg :info :accessor %aframe-info :initform 0)
  (slots :accessor %aframe-slots :initform nil))
 )

(defaclass %simple-frame (%aframe) ())

(defaclass 
 %typed-frame (%aframe) 
 ((frame-class :initarg :frame-class 
               :accessor %typed-frame-frame-class 
               :initform nil))
 )

(defaclass
 %obj ()
 ((value :initarg :value :accessor %obj-value))
 )

(defun new-%obj () (make-instance '%obj :value nil))

(defmacro enable-frame-flag (frame flag)
  `(enable-object-location-flag ,frame %aframe-info ,flag))

(defmacro clear-frame-flag (frame flag)
  `(clear-object-location-flag ,frame %aframe-info ,flag))

(defmacro set-frame-flag (frame flag value)
  `(set-object-location-flag ,frame %aframe-info ,flag ,value))

(defun frame-flag-enabled? (frame flag)
  (flagon? (%aframe-info frame) flag))


;;;; IMPLEMENTATION OF 'FACET' OBJECT (ONE FOR EACH FRAME SLOT)

;;; Cannot use structure because AllegroCache does not allow
;;; structures in persistent store yet.

(progn 
  (defun %fslot-slot (x) (first x))
  (defun %fslot-value (x) (second x))
  (defun %fslot-info (x) (third x))
  (defun %fslot-properties (x) (fourth x))
  (defun set-%fslot-slot (x value) (setf (first x) value))
  (defun set-%fslot-value (x value) (setf (second x) value))
  (defun set-%fslot-info (x value) (setf (third x) value))
  (defun set-%fslot-properties (x value) (setf (fourth x) value))   
  (defsetf %fslot-slot set-%fslot-slot)
  (defsetf %fslot-value set-%fslot-value)
  (defsetf %fslot-info set-%fslot-info)
  (defsetf %fslot-properties set-%fslot-properties)
  (defun new-%fslot (slot value info properties)
    (list slot value info properties))
  (defun %fslot-list (fs) fs)
  (defmacro enable-fslot-flag (fslot flag)
    `(enable-object-location-flag ,fslot fourth ,flag))
  (defmacro clear-fslot-flag (fslot flag)
    `(clear-object-location-flag ,fslot fourth ,flag))
  (defmacro set-fslot-flag (fslot flag value)
    `(set-object-location-flag ,fslot fourth ,flag ,value)))
  
;;; Primitive slot access

(defun %find-fslot (obj slot)
  (loop for fslot in (%obj-value obj) do
        (when (eq slot (%fslot-slot fslot))
          (return fslot)
          )))


(defun find-fslot (frame slot)
  (%find-fslot (%aframe-slots frame) slot))

(defmethod %slotv ((frame %aframe) slot)
  #.(optimization-declaration)
  (declare (type %aframe frame))
  (vwhen (obj (%aframe-slots frame)) 
    (vwhen (fslot (%find-fslot obj slot))
      (%fslot-value fslot)
      )))


;;; Primitive slot setting 

(defmacro %set-existing-fslot (fslot value)
  `(setf (%fslot-value ,fslot) ,value))

(defmacro %add-an-fslot (frame slot value)
  (let ((frame-symbol (gensym "FRAME-"))
        (slot-symbol (gensym "SLOT-"))
        (value-symbol (gensym "VALUE-"))
        (obj-symbol (gensym "OBJ-")))
    `(let ((,frame-symbol ,frame)
           (,slot-symbol ,slot)
           (,value-symbol ,value))
       (vif (,obj-symbol (%aframe-slots ,frame-symbol))
            (%add-another-fslot ,obj-symbol ,slot-symbol ,value-symbol)
            (%add-first-fslot ,frame-symbol ,slot-symbol ,value-symbol)
            ))))

(defmacro %add-first-fslot (frame slot value)
  (let ((obj-symbol (gensym "OBJ-")))
    `(let ((,obj-symbol (new-%obj)))
       (setf (%aframe-slots ,frame) ,obj-symbol)
       (%add-another-fslot ,obj-symbol ,slot ,value)
       )))

(defmacro %add-another-fslot (obj slot value)
  `(push (new-%fslot ,slot ,value 0 nil) (%obj-value ,obj)))

(defun %set-slotv (frame slot value)
  #.(optimization-declaration)
  (declare (type %aframe frame))
  (vif (obj (%aframe-slots frame))
       (vif (fslot (%find-fslot obj slot))
            (%set-existing-fslot fslot value)
            (%add-another-fslot obj slot value))
       (%add-first-fslot frame slot value))
  value)


(defsetf %slotv %set-slotv)

;;; Returns T if slot actually removed, otherwise NIL.
(defmethod %remove-slot ((frame %aframe) slot)
  (vwhen (obj (%aframe-slots frame))
    (vwhen (fslot (find slot (%obj-value obj) :key '%fslot-slot))
      (setf (%obj-value obj) (delete fslot (%obj-value obj)))
      t)))


(defun %delete-frame (frame)
  (let ((obj (%aframe-slots frame)))
    (when obj (delete-persistent-object obj))
    (delete-persistent-object frame)
    ))


(defun delete-persistent-object (x)
  (when user::*acache-running* (delete-instance x)))
  
(defgeneric slot-iterator (frame function)
  (:documentation
   #.(one-string-nl 
      "Calls FUNCTION, a function of 2 arguments (a slot frame and a"
      "slot value), on each of the slot/value pairs of FRAME, including"
      "the fname slot.")))

(defmethod slot-iterator ((frame %aframe) function)
  #.(optimization-declaration)
  (funcall function *fname-frame* (%aframe-fname frame))
  (vwhen (obj (%aframe-slots frame))
    (loop for fslot in (%obj-value obj) do
          (funcall function (%fslot-slot fslot) (%fslot-value fslot)))))

(declaim (inline framep isframe? fname))

(defun framep (x) 
  "Whether the object is a frame.  A predicate returning T or NIL."
 (typep x '%aframe))

(defun isframe? (x) 
  "Whether the object is a frame.  A predicate returning T or NIL."
  (typep x '%aframe))

(defun fname (frame) (%aframe-fname frame))

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
(cformatt "*** Changing *frames-package* to aframes ***")


(defmethod make-load-form ((frame %aframe) &optional ignore)
  (declare (ignore ignore))
  (let ((fname (or (and (fboundp 'slotv) (slotv frame *pretty-name-frame*)) 
                   (string (%aframe-fname frame)))))
    `(if (find-package :aframes)
         (ecase cl-user::*frames-package* 
           (:aframes (frame-fnamed ,fname t))
           (:frames (forward-package-funcall :frames :frame-fnamed ,fname t)))
       (forward-package-funcall :frames :frame-fnamed ,fname t))))

(defmethod print-object ((frame %aframe) stream)
  (if (fboundp 'print-frame)
      (print-frame frame stream)
    (format stream "#$~A" (%aframe-fname frame))))

(defmethod framedbg ((f %aframe))
  (terpri)
  (formatt "Fname: ~S~%" (%aframe-fname f))
  (formatt "Info : (binary) ~16B~%" (%aframe-info f))
  (formatt "Slots: ~S~%" 
           (vwhen (obj (%aframe-slots f))
             (mapcar '%fslot-list (%obj-value obj))))
  (terpri))

