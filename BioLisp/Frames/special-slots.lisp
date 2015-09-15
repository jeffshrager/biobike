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

(defslot #$putDemons :set-valued? t)
(defslot #$addDemons :set-valued? t)
(defslot #$getter)
(defslot #$computedSlot)
(defslot #$inverse :applicable-to #$Slot)
(defslot #$inheritsThrough)
(defslot #$instanceOf)
(defslot #$HTMLGenerator)

(def-computed-slot (#$accessorFn frame slot)
  ;; we need to make a symbol so setf can work
  (let ((accessor-symbol 
         (intern (one-string (slotv frame #$fName) "-ACCESSOR") :frames))
        (accessor (lambda (target-frame) (slotv target-frame frame)))
        (mutator-symbol 
         (intern (one-string (slotv frame #$fName) "-MUTATOR") :frames))
        (mutator 
         (lambda (target-frame new-val) 
           (setf (slotv target-frame frame) new-val))))
    (setf (symbol-function accessor-symbol) accessor)
    (setf (symbol-function mutator-symbol) mutator)
    ;; necessary to use eval because defsetf is defined as a macro 
    ;; and there is no functional equivalent
    (eval `(defsetf ,accessor-symbol ,mutator-symbol))
    accessor-symbol))

(def-inverse-slot #$subClasses #$isA)
(def-inverse-slot #$parts #$partOf)

(def-transitive-slot #$allIsA #$isA)
(def-transitive-slot #$allSubclasses #$subclasses)
(def-transitive-slot #$allParts #$subclasses)
(def-transitive-slot #$allPartOf #$partOf)

;;; Redefine fname to be a computed slot
(def-computed-slot (#$fName frame slot) (gensym-and-intern frame))
  



