;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

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

;;; Author:  JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-clos-api-symbols*
    '(
      slot-names-and-slot-values
      slot-names-and-initargs
      ))

  (export *utility-clos-api-symbols* (find-package :utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CODE TO RETRIEVE SLOT NAMES AND VALUES FROM CLOS INSTANCE

(defun slots-from-class (class)
  (block nil
    #+:allegro
    (return (mop:class-slots class))
    #+:lispworks 
    (return (hcl:class-slots class))
    (error "SLOT-NAMES-FROM-CLASS not implemented!!")
    ))

(defun slot-instances-from-class (class)
  (handler-case
      (slots-from-class class)
    (error 
     ()
     (block nil
       #+:allegro
       (progn
         (mop:finalize-inheritance class)
         (return (slots-from-class class)))
       )
     (error "Something is wrong!!")
     )))

(defun slot-names-from-slot-instances (instances)
  (mapcar
   (lambda (instance)
     (block nil
       #+:allegro
       (return (slot-value instance 'excl::name)) 
       #+:lispworks
       (return (slot-value instance 'clos::name))
       (error "SLOT-NAMES-FROM-SLOT-INSTANCES not implemented!!")
       ))
   instances
   ))

(defun slot-initargs-from-slot-instances (instances)
  (mapcar
   (lambda (instance)
     (block nil
       #+:allegro
       (return (slot-value instance 'excl::initargs)) 
       #+:lispworks
       (return (slot-value instance 'clos::initargs))
       (error "SLOT-NAMES-FROM-SLOT-INSTANCES not implemented!!")
       ))
   instances
   ))

(defun slot-names-from-class-member (obj)
  (let* ((class (class-of obj)))
    (slot-names-from-slot-instances (slot-instances-from-class class))
    ))

(defun slot-initargs-from-class-member (obj)
  (let* ((class (class-of obj)))
    (slot-initargs-from-slot-instances (slot-instances-from-class class))
    ))

(defun slot-names-and-slot-values 
       (instance &optional (unbound-value :<unbound>))
  #.(one-string-nl
     "Given an instance of a class, return the names of each slot and"
     "the value of each slot in an assoc list:  ((N1 V1) (N2 V2) ...)"
     "If a slot is unbound, UNBOUND-VALUE is used in place of a value.")
  (let ((names (slot-names-from-class-member instance)))
    (mapcar 
     (lambda (name) 
       (list 
        name 
        (if (slot-boundp instance name)
            (slot-value instance name)
          unbound-value
          )))
     names
     )))

(defun slot-names-and-initargs (instance)
  #.(one-string-nl
     "Given an instance of a class, return the names of each slot and"
     "the list of initargs of each slot in an assoc list:"
     "((N1 (I11 I12 ...)) (N2 (I12 I22 ...)) ...)")
  (let ((names (slot-names-from-class-member instance))
        (initargs (slot-initargs-from-class-member instance)))
    (mapcar 'list names initargs)
    ))

