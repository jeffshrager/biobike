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

;;; Author:  JP Massar.

;;;; Define a macro which defines bit-twiddling functions and macros 
;;;; which operate on a designated component of a designated object type.
;;;; The component is assumed to be a bitmask -- something of type
;;;; (unsigned-byte n), where N is arbitrary (but it is probably a good
;;;; idea to have it no larger than a fixnum size).

;;;; A set of symbolic names are provided by the user, one for each
;;;; contiguous set of bits (usually a single bit, but can be any size)
;;;; Routines to clear, set and setf the bits represented by these
;;;; symbolic names are provided, along with a general routine to
;;;; clear, set and setf any one of the bit fields by specifying the
;;;; symbolic name.

;;;; So, suppose we have a defstruct or defclass named FOO, and one
;;;; of its components is supposed to contain an (unsigned-byte 16)
;;;; and is accessed by the function FOO-BITMASK.  Suppose we decide
;;;; to define the following bit fields:  ABIT, a field of 1 bit, and
;;;; BBIT, a field of 2 bits.

;;;; (defstruct foo bitmask other-info)

;;;; The following constants/functions/macros will be created:

;;;; Constants:

;;;; +ABIT-MASK+
;;;; +ABIT-OFFSET+
;;;; +BBIT-MASK+
;;;; +BBIT-OFFSET+

;;;; GET-ABIT (object)        ;;; macro
;;;; BGET-ABIT (bits)         ;;; macro
;;;; GET-ABIT-BOOL (object)   ;;; macro -- converts 1 -> T, 0 -> NIL
;;;; BGET-ABIT-BOOL (bits)    ;;; macro
;;;; SET-ABIT (object)        ;;; set to all 1's
;;;; CLEAR-ABIT (object)      ;;; set to all 0's
;;;; SETF-ABIT (object value)
;;;; SETF-ABIT-BOOL (object value) ;;; converts NIL value to 0, otherwise 1

;;;; GET-BBIT (object)
;;;; BGET-BBIT (bits)
;;;; SET-BBIT (object)        ;;; set to all 1's
;;;; CLEAR-BBIT (object)      ;;; set to all 0's
;;;; SETF-BBIT (object value)

;;;; GET-FOO-PROPERTY (object property)
;;;; SET-FOO-PROPERTY (object property)
;;;; CLEAR-FOO-PROPERTY (object property)
;;;; SETF-FOO-PROPERTY (object property value)


;;; Define a set of mask bits for some object

(defmacro define-symbolic-bitmasks-for-object-component

          ((object-type 
            component-accessor
            &key
            (object-name object-type)
            (bit-function-prefix "")
            (bitmask-type '(unsigned-byte 16))
            (package *package*)
            )
           &rest bit-descriptors
           )

  ;; Canonicalize the bit descriptors
  (setq bit-descriptors
        (mapcar
         (lambda (bd)
           (cond
            ((symbolp bd) (list bd 1))
            ((listp bd)
             (unless (and (= (length bd) 2) 
                          (symbolp (first bd))
                          (integerp (second bd))
                          (plusp (second bd))
                          )
               (error "Bad bit descriptor: ~A" bd))
             bd)
            (t (error "Bad bit descriptor: ~A" bd))
            ))
         bit-descriptors
         ))

  (labels 

      ;; Functions to create all the constant/macro/function names

      ((sname (namestring prefix &optional (suffix ""))
         (intern 
          (surround (string namestring) (string prefix) (string suffix))
          package
          ))
       (constant-mask-name (name) (sname name "+" "-MASK+"))
       (constant-offset-name (name) (sname name "+" "-OFFSET+"))
       (bit-getter-name (name) 
         (sname name (one-string bit-function-prefix "GET-")))
       (bbit-getter-name (name) 
         (sname name (one-string bit-function-prefix "BGET-")))
       (bit-getter-bool-name (name) 
         (sname name (one-string bit-function-prefix "GET-") "-BOOL"))
       (bbit-getter-bool-name (name) 
         (sname name (one-string bit-function-prefix "BGET-") "-BOOL"))
       (bit-setter-name (name)
         (sname name (one-string bit-function-prefix "SET-")))
       (bit-clearer-name (name)
         (sname name (one-string bit-function-prefix "CLEAR-")))
       (bit-setfer-name (name)
         (sname name (one-string bit-function-prefix "SETF-")))
       (bit-setfer-bool-name (name)
         (sname name (one-string bit-function-prefix "SETF-") "-BOOL"))
       (symbolic-getter-name () (sname object-name "GET-" "-PROPERTY"))
       (symbolic-setter-name () (sname object-name "SETF-" "-PROPERTY"))
       ;; Internal function
       (bit-setf-name () (sname object-name "%INTERNAL-SETF-" "-BITS"))
       )

    `(progn
       ;; Define individual get, setf, set and clear functions for each bit,
       ;; and the constant masks and offsets.
       ,@(loop with generic-bit-setfer = (bit-setf-name)
               with offset = 0
               for (name width) in bit-descriptors
               as bits = (1- (expt 2 width))
               as mask = (ash bits offset)
               as s = (string name)
               as mask-name = (constant-mask-name s)
               as offset-name = (constant-offset-name s)
               as getter = (bit-getter-name s)
               as bit-getter = (bbit-getter-name s)
               as bool-getter = (bit-getter-bool-name s)
               as bool-bit-getter = (bbit-getter-bool-name s)
               as setter = (bit-setter-name s)
               as clearer = (bit-clearer-name s)
               as setfer = (bit-setfer-name s)
               as bool? = (= width 1)
               append
               `(
                 (declaim (type ,bitmask-type ,mask-name))
                 (declaim (type fixnum ,offset-name))
                 (defconstant ,mask-name ,mask)
                 (defconstant ,offset-name ,offset)
                 ;; The getter's are macros to be as fast as possible
                 (defmacro ,bit-getter (bits)
                   `(ash (the fixnum (logand ,bits ,',mask)) ,',(- offset)))
                 ,@(when bool?
                     `((defmacro ,bool-bit-getter (bits)
                         `(plusp (the fixnum (logand ,bits ,',mask))))))
                 (defmacro ,getter (obj)
                   (let ((bits (gensym "BITS-")))
                     `(let ((,bits
                             (the ,',bitmask-type
                                  (,',component-accessor
                                   (the ,',object-type ,obj)
                                   ))))
                        (declare (type ,',bitmask-type bits))
                        (,',bit-getter ,bits)
                        )))
                 ,@(when bool?
                     `((defmacro ,bool-getter (obj)
                         (let ((bits (gensym "BITS-")))
                           `(let ((,bits
                                   (the ,',bitmask-type
                                        (,',component-accessor
                                         (the ,',object-type ,obj)
                                         ))))
                              (declare (type ,',bitmask-type bits))
                              (,',bool-bit-getter ,bits)
                              )))))
                 (defun ,setter (obj)
                   (,generic-bit-setfer obj ,mask-name ,offset-name ,bits))
                 (defun ,clearer (obj)
                   (,generic-bit-setfer obj ,mask-name ,offset-name 0))
                 (defun ,setfer (obj value)
                   (,generic-bit-setfer obj ,mask-name ,offset-name value))
                 )
               do (incf offset width)
               )

       ;; Define a 'generic' function to set a bit based on a mask.
       (defun ,(bit-setf-name) (obj mask offset value)
         (declare (type ,object-type obj) (type ,bitmask-type mask))
         (let ((current-value (,component-accessor obj))
               (new-shifted-value 
                (the ,bitmask-type (ash value (the fixnum offset)))
                ))
           (declare (type ,bitmask-type current-val new-shifted-value))
           (setf (,component-accessor obj)
                 (the 
                  ,bitmask-type
                  (logior 
                   new-shifted-value
                   (the ,bitmask-type
                        (logand current-value (the ,bitmask-type (lognot mask)))
                        ))))
           value
           ))

       ;; Define a get function based on the symbolic name of the bit
       (defun ,(symbolic-getter-name) 
              (obj property &optional (return-single-bit-as-boolean? t))
         (case property
           ,@(loop for (name width) in bit-descriptors
                   as s = (string name)
                   as key = (keywordize s) 
                   as getter = (bit-getter-name s)
                   as bool-getter = (bit-getter-bool-name s)
                   as bool? = (= width 1)
                   collect 
                   (if bool?
                       `(,key
                         (if return-single-bit-as-boolean?
                             (,bool-getter obj)
                           (,getter obj)
                           ))
                     `(,key (,getter obj))
                     ))
           (otherwise 
            (error "~S is not a defined property for ~A!" property obj))
           ))

       ;; Define a setf function based on the symbolic name of the bit
       (defun ,(symbolic-setter-name) 
              (obj property value 
                   &optional (interpret-single-bit-as-boolean? t))
         (multiple-value-bind (bool? mask offset limit)
             (case property
               ,@(loop for (name width) in bit-descriptors
                       as limit = (expt 2 width)
                       as s =  (string name)
                       as key = (keywordize s) 
                       as mask-name = (constant-mask-name s)
                       as offset-name = (constant-offset-name s)
                       collect 
                       `(,key 
                         (values ,(= width 1) ,mask-name ,offset-name ,limit)
                         ))
               (otherwise 
                (error "~S is not a valid property for ~A!" property obj))
               )
           (,(bit-setf-name) 
            obj mask offset
            (cond
             ((null interpret-single-bit-as-boolean?) value)
             (bool? (if value 1 0))
             ((and (integerp value) (not (minusp value)) (< value limit))
              value)
             (t (error "Invalid value, ~S, for property ~S" value property))
             ))))

       )))

