;;; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

#|
Start at systematic indexing
Mike Travers, 2003/4

indexedSlot
- uses EQUALP as default test (for case-insensitive index on strings)
- single values
- unique
- doesn't do uniqueness checking (yet)

If a slot S is defined as being a #$sys.indexedSlot, then the frame
representing that slot contains a hash table, H in its #$sys.indexer slot.
Whenever a value V is stored as the value of slot S within some frame F
then F is pushed on to the list of hash table values in H keyed by V.

So the point is that given a V, we can determine all the frames which
contain V as the value of the frame's slot S.

So let's say we had a slot named #$Result-Molecule which was meant to contain
the single result of a chemical reaction.

If we were to define #$Result-Molecule as an indexedSlot then once we had
filled that slot in all our frames which denoted chemical reactions we could
determine all the chemical reaction frames which had, for example,
H20 molecules as the result of the reaction by evaluating:

(slot-lookup #$sys.H20 #$sys.Result-Molecule)

|#


(defslot #$sys.indexedSlot)

(def-inherited-slot #$sys.equalityTest)

(setf (slotv #$sys.indexedSlot #$sys.equalityTest) 'equalp)

(add-put-demon #$sys.indexedSlot 'indexed-slot-put-daemon)

(defun indexed-slot-put-daemon (frame slot value)
  (error "Does not work anymore.")
  (let ((ht (slotv slot #$sys.indexer)))
    (when (null ht)
      (setq ht (make-indexer 
                *frame-table* slot (slotv slot #$sys.equalityTest)))
      (setf (slotv slot #$sys.indexer) ht))
    (if (null (slotv slot #$sys.SetValued))
        (add-index ht frame value)
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
          (add-index ht frame (first new-list)))
         ;; Setting the value of the slot to a (possibly completely) new
         ;; list of values.  Index them.
         (t (loop for v in new-list do (add-index ht frame v)))
         )))))

(defmethod make-indexer ((ft ac-map) slot test)
  (when (eq test 'equalp)
    (error "Cannot do EQUALP map yet."))
  (open-map (one-string "MAP-~A" (slotv slot #$fname))
            :if-does-not-exist :create
            :if-exists :error))

(defmethod make-indexer ((ft hash-table) slot test)
  (declare (ignore slot))
  (make-hash-table :test test))

(defun get-index (indexer value)
  (fmap-value indexer value))

(defun add-index (indexer frame value)
  (add-fmap-value indexer frame value))

(defun slot-lookup (value slot)
  (fmap-value (slotv slot #$sys.indexer) value))

(defun def-indexed-slot (slot &key test)
  ;; Make SLOT inherit #$sys.EqualityTest from #$sys.IndexedSlot 
  ;; if not provided.
  (defslot slot :base #$sys.indexedSlot)
  (setf (slotv slot #$sys.putDemons) (slotv #$sys.indexedSlot #$sys.putDemons))
  (when test 
    (setf (slotv slot #$sys.equalityTest) 
          (cond
           ((member test (list #'eq 'eq)) 'eq)
           ((member test (list #'eql 'eql)) 'eql)
           ((member test (list #'equal 'equal)) 'equal)
           ((member test (list #'equalp 'equalp)) 'equalp)
           ((member test (list #'string= 'string=)) 'equal)
           ((member test (list #'string-equal 'string-equal)) 'equalp)
           (t (error "Invalid TEST for DEF-INDEXED-SLOT: ~A" test))
           ))))


