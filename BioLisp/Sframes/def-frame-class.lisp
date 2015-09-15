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

(defstruct fsdesc slot name symbol accessor initarg allocation domain initform)

(defun parse-frame-slot-descriptor (class-name d)
  (setq d (ensure-list d))
  (destructuring-bind (name &key (allocation :instance) (domain t) initform) d
    (unless (member allocation '(:instance :class))
      (error 
       "Illegal :ALLOCATION frame class slot option value: ~S" allocation))
    (unless (or (member domain '(:set :list))
                (and (symbolp domain) (not (keywordp domain))))
      (error "Illegal :DOMAIN frame class slot option value: ~S" domain))
    (let* ((slot (frame-fnamed (fstring name) t))
           (sname (fname slot))
           (symbol (intern (string-upcase sname) :aframes)))
      (make-fsdesc 
       :slot slot 
       :name sname
       :symbol symbol
       :accessor (frame-class-slot-accessor-symbol class-name sname)
       :initarg (frame-class-slot-initarg-symbol class-name sname)
       :allocation allocation
       :domain domain
       :initform initform
       ))))
      
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun intern-upcase (string package)
  (intern (string-upcase string) package))

#+not-called
(defun frame-class-slot-name-symbol (frame-name slot-name)
  (intern-upcase
    (one-string "$!-" frame-name "-" slot-name "-!$")
    *package*
    ))

(defun frame-class-slot-accessor-symbol (frame-name slot-name)
  (intern-upcase
    (one-string "$!-" frame-name "-" slot-name "-accessor-internal-!$")
    #-oops
    (find-package :aframes)
    #+oops
    *package*
    ))

(defun frame-class-slot-initarg-symbol (frame-name slot-name)
  (intern-upcase (one-string frame-name "-" slot-name) :keyword))

)

(defmacro def-frame-class 
          (frame-type (&rest frame-classes) (&rest fixed-slots))
  (labels ((to-upcase-symbol (x) 
             (intern-upcase (fstring x) (find-package :aframes)))
           (to-frame (x) (if (isframe? x) x (frame-fnamed (string x) t))))
    (setq frame-type (to-frame frame-type))
    (let* ((new-class-symbol (to-upcase-symbol (fstring frame-type)))
           (new-class-string (string new-class-symbol))
           (inheriting-class-symbols (mapcar #'to-upcase-symbol frame-classes))
           (inheriting-class-frames 
            (mapcar #'to-frame inheriting-class-symbols))
           (slot-descriptors
            (mapcar 
             (lambda (x) (parse-frame-slot-descriptor new-class-string x))
             fixed-slots
             ))
           (class-slot-descriptors
            (remove-if-not 
             (lambda (x) (eq (fsdesc-allocation x) :class))
             slot-descriptors
             ))
           (instance-slot-descriptors 
            (remove-if-not 
             (lambda (x) (eq (fsdesc-allocation x) :instance))
             slot-descriptors
             ))
           )
      `(progn
         (defaclass ,new-class-symbol
                   (,@(if frame-classes 
                          inheriting-class-symbols
                        (list '%typed-frame)))
           ,(mapcar
              (lambda (fs)
                `(,(fsdesc-symbol fs) 
                  :initarg ,(fsdesc-initarg fs) 
                  :accessor ,(fsdesc-accessor fs)))
              instance-slot-descriptors
              ))
         ,(make-frame-class-frame 
          frame-type new-class-symbol inheriting-class-frames fixed-slots
          class-slot-descriptors instance-slot-descriptors)
         ,(generate-typed-instance-initialization-method 
           new-class-symbol frame-type instance-slot-descriptors)
         ,(generate-%fixed-slotv-method 
           new-class-symbol instance-slot-descriptors)
         ,(generate-%fixed-set-slotv-method 
           new-class-symbol instance-slot-descriptors)
         ,(generate-%fixed-slot?-method 
           new-class-symbol instance-slot-descriptors)
         ,(generate-slot-iterator-method 
           new-class-symbol instance-slot-descriptors)
         ,frame-type
          ))))

(defun make-frame-class-frame 
       (frame new-class-symbol inheriting-class-frames fixed-slots
              class-slot-descriptors instance-slot-descriptors)
  `(progn 
     ,@(loop for csd in class-slot-descriptors collect
             `(setf (slotv ,frame ,(fsdesc-slot csd)) ,(fsdesc-initform csd)))
     (setf (slotv ,frame #$sys.frame-class-symbol) ',new-class-symbol)
     (setf (slotv ,frame #$sys.inheriting-frame-classes) 
           ',inheriting-class-frames)
     ,@(loop for iframe in inheriting-class-frames collect
             `(pushnew ,frame (slotv ,iframe #$sys.frame-subclasses)))
     (setf (slotv ,frame #$sys.fixed-slots) ',fixed-slots)
     (setf (slotv ,frame #$sys.class-slots) 
           ',(mapcar 'fsdesc-slot class-slot-descriptors))
     (setf (slotv ,frame #$sys.instance-slots)
           ',(mapcar 'fsdesc-slot instance-slot-descriptors))
     (setf (get ',new-class-symbol :class-frame) ,frame)
     ))

(defun frame-class-frame (frame-class-symbol &key (if-does-not-exist nil))
  #.(one-string-nl
     "Returns frame associated with the frame class type FRAME-CLASS-SYMBOL."
     "If no such frame exists (which should imply that no such frame class was"
     "ever defined) IF-DOES-NOT-EXIST controls what happens:"
     "If NIL, NIL is returned."
     "If :warn, a warning is issued and NIL is returned."
     "If :error, an error is signalled.")
  (or (get frame-class-symbol :class-frame) 
      (let ((class-frame (frame-fnamed (symbol-name frame-class-symbol))))
        (cond
         ((null class-frame) 
          (ecase if-does-not-exist 
            ((nil) nil)
            (:warn 
             (warn "Frame class ~S does not exist." frame-class-symbol) nil)
            (:error 
             (error "Frame class ~S does not exist." frame-class-symbol))
            ))
         ((null (slotv class-frame #$sys.frame-class-symbol))
          (ecase if-does-not-exist 
            ((nil) nil)
            (:warn (warn "Frame ~S exists but is not a frame class symbol." 
                         frame-class-symbol) nil)
            (:error 
             (error "Frame class ~S exists but is not a frame class symbol."
                    frame-class-symbol))))
         (t 
          (setf (get frame-class-symbol :class-frame) class-frame)
          class-frame)
         ))))

(defun add-instance-to-frame-class (instance frame-class-frame)
  (push instance (slotv frame-class-frame #$sys.instances)))
          

        
  




