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

#|
Start at systematic indexing
Mike Travers, 2003/4

indexedSlot
- uses EQUALP as default test (for case-insensitive index on strings)
- single values
- unique
- doesn't do uniqueness checking (yet)

If a slot S is defined as being a #$indexedSlot, then the frame
representing that slot contains a hash table, H in its #$hashtable slot.
Whenever a value V is stored as the value of slot S within some frame F
then F is pushed on to the list of hash table values in H keyed by V.

So the point is that given a V, we can determine all the frames which
contain V as the value of the frame's slot S.

So let's say we had a slot named #$Result-Molecule which was meant to contain
the single result of a chemical reaction.

If we were to define #$Result-Molecule as an indexedSlot then once we had
filled that slot in all our frames which denoted chemical reactions we could
determine which all the chemical reaction frames which had, for example,
H20 molecules as the result of the reaction by evaluating:

(slot-lookup #$H20 #$Result-Molecule)

|#

(defslot #$indexedSlot)

(def-inherited-slot #$equalityTest)
(setf (slotv #$indexedSlot #$equalityTest) 'equalp)

(defun indexed-slot-put-daemon (frame slot value)
   (let ((ht (slotv slot #$hashtable)))
     (when (null ht)
       (setq ht (make-hash-table :test (slotv slot #$equalityTest)))
       (setf (slotv slot #$hashtable) ht))
     (if (null (slotv slot #$SetValued))
         (pushnew frame (gethash value ht))
       (let ((old-list (slotv frame slot))
             (new-list value))
         (unless (listp new-list)
           (setq new-list 
                 (handle-non-list-for-set-valued-slot frame slot value)))
         (cond
          ;; Triggered from a PUSHNEW of a value already an element
          ;; of the slot's set.  No need to do anything.
          ((eq new-list old-list) nil)
          ;; Triggered from a PUSHNEW or equivalent of a value not already 
          ;; an element of the slot's set.  The new value is the first
          ;; element of NEW-LIST.  Index it.
          ((eq (cdr new-list) old-list) 
           (pushnew frame (gethash (first new-list) ht)))
          ;; Setting the value of the slot to a (possibly completely) new
          ;; list of values.  Index them.
          (t (loop for v in new-list do (pushnew frame (gethash v ht))))
          )))))


;; This should never be called, since we've changed ADD-ELEMENT.

(defun indexed-slot-add-daemon (frame slot value)
   (let ((ht (slotv slot #$hashtable)))
     (when (null ht)
       (setq ht (make-hash-table :test (slotv slot #$equalityTest)))
       (setf (slotv slot #$hashtable) ht))
     (if (null (slotv slot #$SetValued))
         (error 
          (one-string
           "Internal error. ADD Daemon triggered on non-set-valued slot ~A,"
           "frame ~A.  Value being added is ~A")
          slot frame value)
       (pushnew frame (gethash value ht))
       )))

(add-put-demon #$indexedSlot 'indexed-slot-put-daemon)
(add-add-demon #$indexedSlot 'indexed-slot-add-daemon)

(defun slot-lookup (value slot)
  (gethash value (slotv slot #$hashtable)))

(defun def-indexed-slot (slot &key test)
  ;; Make SLOT inherit #$EqualityTest from #$IndexedSlot if not provided.
  (defslot slot :base #$indexedSlot)
  (setf (slotv slot #$putDemons) (slotv #$indexedSlot #$putDemons))
  (setf (slotv slot #$addDemons) (slotv #$indexedSlot #$addDemons))
  (when test 
    (setf (slotv slot #$equalityTest) 
          (cond
           ((member test (list #'eq 'eq)) 'eq)
           ((member test (list #'eql 'eql)) 'eql)
           ((member test (list #'equal 'equal)) 'equal)
           ((member test (list #'equalp 'equalp)) 'equalp)
           ((member test (list #'string= 'string=)) 'string=)
           ((member test (list #'string-equal 'string-equal)) 'string-equal)
           (t (error "Invalid TEST for DEF-INDEXED-SLOT: ~A" test))
           ))))


