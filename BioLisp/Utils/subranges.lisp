;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils; -*-

(in-package :utils) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Implementation of SUBRANGE creation and setf'ing

;;; (subrange "abcdefghij" 0 (3 5) (8 9)) --> "adefij"
;;; (let ((x "abcdefghij"))
;;;   (setf (subrange x 1 (5 3)) "wxyz")
;;;   x)
;;; --> "awczyxghij"

;;; A range argument (x y) is inclusive, not exclusive as with SUBSEQ.

;;; A range argument (x y) can have either x <= y or y <= x, but both
;;; x and y must be greater than or equal to the base (0 or 1, depending
;;; on whether SUBRANGES or SUBRANGES-OF, respectively, is called)
;;; and less than or equal to the maximum allowed index of the input sequence.

;;; An integer argument i must satisfy the same requirements: (>= i base) and 
;;; (<= i (maximum-index-of-sequence sequence))

;;; If *subrange-safety* is enables these constraints are checked
;;; and an error signalled if a constraint is violiated;
;;; if not, the result is undefined if the constraints are not satisfied.

;;; The set of indices defined by the range and index arguments need not
;;; be unique; overlap is possible.

;;; If a range (x y) has x > y it is a 'reversed' range.  A reversed range
;;; works identically to a regular range except the indices are reversed:
;;; wherease a normal range (3 5) behavior as if it were three integer
;;; indices 3, 4 and 5, a reversed range (5 3) behaves as if it were
;;; 5, 4 and 3.

;;; If the operation is performed on a vector the time should be approximately
;;; proportional to the number of elements retrieved.
;;; If the operation is performed on a list the time depends on whether
;;; or not all the indices are ordered and disjoint:  if they are, the
;;; time should be approximately  proportional to the highest index present;
;;; If not, the time is a complicated function, approximately proportional
;;; to the square of the number of indices retrieved modulated by the highest
;;; index present.


(defgeneric subranges-of (sequence &rest indices-and-ranges))
(defgeneric subranges (sequence &rest indices-and-ranges))
(defgeneric (lisp:setf subranges-of) 
    (new-values sequence &rest indices-and-ranges))
(defgeneric (lisp:setf subranges) 
    (new-values sequence &rest indices-and-ranges))

(defmethod subranges-of ((sequence vector) &rest indices-and-ranges)
  (subranges-vector-internal sequence indices-and-ranges 1 "SUBRANGES-OF"))

(defmethod subranges ((sequence vector) &rest indices-and-ranges)
  (subranges-vector-internal sequence indices-and-ranges 0 "SUBRANGES"))

(defmethod subranges-of ((sequence list) &rest indices-and-ranges)
  (subranges-list-internal sequence indices-and-ranges 1 "SUBRANGES-OF"))

(defmethod subranges ((sequence list) &rest indices-and-ranges)
  (subranges-list-internal sequence indices-and-ranges 0 "SUBRANGES"))

(defmethod (lisp:setf subranges-of)
           ((value t) (sequence vector) &rest indices-and-ranges)
  (setf-subranges-vector-internal
   value sequence indices-and-ranges 1 "(SETF SUBRANGES-OF)"))

(defmethod (lisp:setf subranges)
           ((value t) (sequence vector) &rest indices-and-ranges)
  (setf-subranges-vector-internal
   value sequence indices-and-ranges 0 "(SETF SUBRANGES)"))

(defmethod (lisp:setf subranges-of)
           ((value t) (sequence list) &rest indices-and-ranges)
  (setf-subranges-list-internal
   value sequence indices-and-ranges 1 "(SETF SUBRANGES-OF)"))

(defmethod (lisp:setf subranges)
           ((value t) (sequence list) &rest indices-and-ranges)
  (setf-subranges-list-internal
   value sequence indices-and-ranges 0 "(SETF SUBRANGES)"))


(defun subranges-vector-internal (v ir base name)
  (when *safety*
    (check-subrange-of-indices-and-ranges v ir base :vector name))
  (subranges-of-aux 
   v (if (stringp v) 'simple-string 'simple-vector) ir base name))

(defun setf-subranges-vector-internal (value v ir base name)
  (when *safety*
    (check-subrange-of-indices-and-ranges v ir base :vector name))
  (setf-subranges-of-aux 
   value v (if (stringp v) 'simple-string 'simple-vector) ir base name))



(defun subranges-list-internal (sequence indices-and-ranges base name)
  #.(optimization-declaration)
  (declare (list sequence))
  (declare (fixnum base))
  (when *safety*
    (check-subrange-of-indices-and-ranges 
     sequence indices-and-ranges base :list name))
  (macrolet ((-tf (x y) `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))
             (+tf (x y) `(the fixnum (+ (the fixnum ,x) (the fixnum ,y)))))
    (labels ((last-index () (-tf (length sequence) (1- base)))
             (oops-bad-range-value (v r)
               (error
                (one-string-nl
                 "Error in ~A!"
                 "The index ~S, in the subrange ~S, exceeds"
                 "the index of the last element of the input list, ~S !"
                 "You cannot use ~A to index beyond"
                 "the end of a list...")
                name v r (last-index) name
                )))               
      (if (not (in-order-and-exclusive? indices-and-ranges))
          (subranges-of-aux sequence 'list indices-and-ranges base name)
        (let ((results nil)
              (rest-of-list sequence)
              (current-head-index base))
          (declare (fixnum current-head-index))
          (loop 
           for ir in indices-and-ranges do
           (cond
            ((integerp ir)
             (let ((delta (-tf ir current-head-index)))
               (declare (fixnum delta))
               (setf rest-of-list (nthcdr delta rest-of-list))
               (when (null rest-of-list)
                 (error
                  (one-string-nl
                   "Error in ~A!"
                   "The index ~S exceeds the index of the last element"
                   "of the input list, ~S ! You cannot use ~A to index"
                   "beyond the end of a list.")
                  name ir (last-index)
                  ))
               (setq current-head-index (+tf current-head-index delta))
               (push (first rest-of-list) results)))
            (t
             (let ((i1 (first ir)) (i2 (second ir)))
               (declare (fixnum i1 i2))
               (if (<= i1 i2) 
                   ;; step 1: pop the list until we get to index i1
                   ;; step 2: push elements of the list onto RESULTS, 
                   ;; popping the list as we go along.  
                   (let ((delta (-tf i1 current-head-index)))
                     (declare (fixnum delta))
                     (setf rest-of-list (nthcdr delta rest-of-list))
                     (when (null rest-of-list)
                       (oops-bad-range-value i1 ir))
                     (setq current-head-index (+tf current-head-index delta))
                     (loop 
                      for j fixnum from i1 to i2
                      do
                      (when (null rest-of-list)
                        (oops-bad-range-value i2 ir))
                      (push (first rest-of-list) results)
                      (pop rest-of-list)
                      (setq current-head-index (+tf current-head-index 1))
                      ))
                 ;; the range is a reverse range...
                 ;; step 1: pop the list until we get to index i2, 
                 ;; which is the lesser of the indices. 
                 ;; step 2: collect elements of the list from index i2 to 
                 ;; index i1 (the greater of the indices), then append them
                 ;; to the beginning of RESULTS
                 (let ((delta (-tf i2 current-head-index)))
                   (declare (fixnum delta))
                   (setf rest-of-list (nthcdr delta rest-of-list))
                   (when (null rest-of-list)
                     (oops-bad-range-value i2 ir))
                   (setq current-head-index (+tf current-head-index delta))
                   (setq 
                    results 
                    (nconc
                     (loop for j fixnum from i2 to i1 
                           collect
                           (progn
                             (when (null rest-of-list)
                               (oops-bad-range-value i1 ir))
                             (prog1
                                 (first rest-of-list)
                               (pop rest-of-list)
                               (setq current-head-index 
                                     (+tf current-head-index 1))
                               )))
                     results
                     ))))))))
          (nreverse results)
          )))))


(defun setf-subranges-list-internal 
       (value sequence indices-and-ranges base name 
              &aux 
              value-is-scalar? (value-length 0) (value-index 0)
              rest-of-list current-head-index
              )              
  #.(optimization-declaration)
  (declare (list sequence))
  (declare (fixnum base value-index value-length))

  (when *safety*
    (check-subrange-of-indices-and-ranges 
     sequence indices-and-ranges base :list name))

  (when (not (in-order-and-exclusive? indices-and-ranges))
    (setf-subranges-of-aux value sequence 'list indices-and-ranges base name)
    (return-from setf-subranges-list-internal value))
  
  (setq value-is-scalar? (and (not (listp value)) (not (vectorp value))))
  (unless value-is-scalar? (setq value-length (length value)))
  (setq rest-of-list sequence)
  (setq current-head-index base)

  (macrolet ((-tf (x y) `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))
             (+tf (x y) `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))
             (-base (x) `(the fixnum (- (the fixnum ,x) base))))
    (labels ((last-index () (-tf (length sequence) (1- base)))
             (oops-bad-range-value (v r)
               (error
                (one-string-nl
                 "Error in ~A!"
                 "The index ~S, in the subrange ~S, exceeds"
                 "the index of the last element of the input list, ~S !"
                 "You cannot use ~A to index beyond"
                 "the end of a list...")
                name v r (last-index) name 
                ))
             (set-next (vi)
               (if value-is-scalar? 
                   (setf (first rest-of-list) value)
                 (progn 
                   (when (>= vi value-length)
                     (error 
                      (one-string-nl
                       "Error in ~A!"
                       "The value sequence, ~S,"
                       "has ~D elements in it, but you are trying to set more"
                       "elements than that in the sequence to be changed."
                       "You cannot change more elements than there are values"
                       "to set them with.")
                      name value value-length))
                   (setf (first rest-of-list) (elt value vi))
                   ))))
      (loop 
       for ir in indices-and-ranges do
       (cond
        ((integerp ir)
         (let ((delta (-tf ir current-head-index)))
           (declare (fixnum delta))
           (setf rest-of-list (nthcdr delta rest-of-list))
           (incf current-head-index delta)
           (when (null rest-of-list)
             (error
              (one-string-nl
               "Error in ~A!"
               "The index ~S exceeds the index of the last element"
               "of the input list, ~S ! You cannot use ~A to index"
               "beyond the end of a list.")
              name ir (last-index)
              ))
           (set-next value-index)
           (incf value-index)
           ))
        (t
         (let ((i1 (first ir)) (i2 (second ir)))
           (declare (fixnum i1 i2))
           (if (<= i1 i2) 
               ;; step 1: pop the list until we get to index i1
               ;; step 2: setf the car of the storage list with the 
               ;; next element from VALUE, popping the list as we go along.  
               (let ((delta (-tf i1 current-head-index)))
                 (declare (fixnum delta))
                 (setf rest-of-list (nthcdr delta rest-of-list))
                 (setq current-head-index (+tf current-head-index delta))
                 (when (null rest-of-list) (oops-bad-range-value i1 ir))
                 (loop 
                  for j fixnum from (-base i1) to (-base i2)
                  do
                  (when (null rest-of-list)
                    (oops-bad-range-value i2 ir))
                  (set-next value-index)
                  (pop rest-of-list)
                  (incf value-index) 
                  (setq current-head-index (+tf current-head-index 1))
                  ))
             ;; the range is a reverse range...
             ;; step 1: pop the list until we get to index i2, 
             ;; which is the lesser of the indices. 
             ;; step 2: setf the car of the storage list with elements 
             ;; taken from VALUE in reverse order, popping the list as
             ;; we go along.
             (let ((delta (-tf i2 current-head-index))
                   (subseq-size (the fixnum (1+ (-tf i1 i2)))))
               (declare (fixnum delta subseq-size))
               (setf rest-of-list (nthcdr delta rest-of-list))
               (when (null rest-of-list)
                 (oops-bad-range-value i2 ir))
               (setq current-head-index (+tf current-head-index delta))
               (loop 
                for j fixnum from i2 to i1 
                for vi fixnum 
                from (the fixnum (1- (+tf value-index subseq-size)))
                downto value-index 
                do
                (when (null rest-of-list) (oops-bad-range-value i1 ir))
                (set-next vi)
                (pop rest-of-list)
                (incf value-index)
                (setq current-head-index (+tf current-head-index 1))
                )))))))
          
      value
      
      )))

(defun check-subrange-of-indices-and-ranges (seq i&r base type name)
  (declare (fixnum base))
  #.(optimization-declaration)
  (flet ((maybe-oops-below-base (i &optional (range nil))
           (declare (fixnum i))
           (unless (>= i base)
             (error 
              (one-string-nl
               "In a call to ~A~A~A"
               "the index value, ~S, is less than ~S !! Not allowed!")
              name
              (if range ", in one of the argument ranges," "")
              (if range (formatn " ~S," range) "")
              i base)))
         (maybe-oops-beyond-limit (limit i &optional (range nil))
           (declare (fixnum limit i))
           (unless (<= i limit)
             (error 
              (one-string-nl
               "In a call to ~A~A~A"
               "the index value, ~S, is more than ~S, the highest legal index"
               "for the input vector.  You cannot use ~A to index off the"
               "end of a sequence!")
              name
              (if range ", in one of the argument ranges," "")
              (if range (formatn " ~S," range) "")
              i limit name
              )))
         (maybe-oops-not-integer (i range)
           (unless (integerp i)
             (error
              (one-string-nl
               "In a call to ~A, in one of the argument ranges, ~S,"
               "the index value, ~S, is not an integer!")
              name range i))))
    (loop for x in i&r do
          (cond
           ((integerp x)
            (maybe-oops-below-base x nil))
           ((listp x)
            (maybe-oops-not-integer (first x) x)
            (maybe-oops-not-integer (second x) x)
            (maybe-oops-below-base (first x) x)
            (maybe-oops-below-base (second x) x)
            (let ((i1 (first x)) (i2 (second x)))
              (declare (ignore i1 i2))
              (declare (fixnum i1 i2))
              (unless (null (cddr x))
                (error 
                 (one-string-nl
                  "In a call to ~A,"
                  "the argument ~S is not a valid range!  It must be a list"
                  "of exactly two elements, both integers.")
                 name x))
              #+not-with-reversed-ranges
              (unless (<= i1 i2)
                (error
                 (one-string-nl
                  "In a call to ~A,"
                  "the argument ~S is not a valid range!  It must be a list"
                  "of two integers, the first less than or equal to the second")
                 name x
                 ))))
           (t
            (error
             (one-string-nl
              "In a call to ~A,"
              "the argument ~S is not a valid index or range argument."
              "The argument must be an integer or list of two integers;"
              "instead, it is of type ~S.")
             name x (printed-type-of x)             
             ))))
    (ecase type
      (:list nil)
      (:vector
       (let* ((lastindex (the fixnum (1- (the fixnum (length seq)))))
              (limit (the fixnum (+ lastindex base))))
         (declare (fixnum lastindex limit))
         (loop 
          for x in i&r do
          (cond
           ((integerp x)
            (maybe-oops-beyond-limit limit x nil))
           ((listp x) 
            (maybe-oops-beyond-limit limit (first x) x)
            (maybe-oops-beyond-limit limit (second x) x)
            ))))))))

(defun subranges-of-aux (sequence result-type indices-and-ranges base name)
  (declare (fixnum base))
  (declare (ignore name))
  #.(optimization-declaration)
  (macrolet ((-tf (x y) `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))
             (-base (x) `(the fixnum (- (the fixnum ,x) base))))
    (apply
     'concatenate
     result-type
     (mapcar
      (lambda (x)
        (cond 
         ((integerp x) 
          (list (elt sequence (-base x))))
         ((listp x) 
          (let ((i1 (first x)) (i2 (second x)))
            (declare (fixnum i1 i2))
            (if (<= i1 i2)
                (let ((subseq-start (-tf i1 base))
                      (subseq-end (1+ (-tf i2 base))))
                  (declare (fixnum subseq-start subseq-end))
                  (subseq sequence subseq-start subseq-end))
              (let ((subseq-start (-tf i2 base))
                    (subseq-end (1+ (-tf i1 base))))
                (nreverse (subseq sequence subseq-start subseq-end))
                ))))
         (t (error "Internal error. Should be caught previously!"))
         ))
      indices-and-ranges
      ))))

(defun setf-subranges-of-aux 
       (value sequence result-type indices-and-ranges base name)
  (declare (fixnum base) (ignore result-type))
  #.(optimization-declaration)
  (macrolet ((-tf (x y) `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))
             (+tf (x y) `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))
             (-base (x) `(the fixnum (- (the fixnum ,x) base))))
    (loop 
     with value-index fixnum = 0
     with value-is-scalar? = (not (or (listp value) (vectorp value)))
     with vlen fixnum = (if value-is-scalar? -1 (length value))
     for x in indices-and-ranges 
     do
     (flet ((enough-elements? (v)
              (declare (fixnum v))
              (when (not value-is-scalar?) 
                (when (>= v vlen)
                  (error 
                   (one-string-nl
                    "In a call to ~A,"
                    "you are trying to set ~D or more elements, but you only"
                    "provided ~D elements to use.  You must provide at least"
                    "as many elements as you want to set.")
                   name (the fixnum (1+ v)) vlen)))))
       (cond 
        ((integerp x) 
         (enough-elements? value-index)
         (setf (elt sequence (-base x)) 
               (if value-is-scalar? value (elt value value-index)))
         (incf value-index))
        ((listp x) 
         (let ((i1 (first x)) (i2 (second x)))
           (declare (fixnum i1 i2))
           (if (<= i1 i2)
               (let* ((subseq-start (-base i1))
                      (subseq-end (1+ (-base i2)))
                      (delta (-tf subseq-end subseq-start)))
                 (declare (fixnum subseq-start subseq-end delta))
                 (if (not value-is-scalar?) 
                     (let ((value-range-end (+tf value-index delta)))
                       (declare (fixnum value-range-end))
                       (enough-elements? (the fixnum (1- value-range-end)))
                       (setf (subseq sequence subseq-start subseq-end)
                             (subseq value value-index value-range-end))
                       (incf value-index delta))
                   (loop for j fixnum from subseq-start below subseq-end 
                         do 
                         (setf (elt sequence j) value)
                         (incf value-index) 
                         )))
             ;; reversed range
             (let ((subseq-start (-base i2))
                   (subseq-end-inclusive (-base i1)))
               (declare (fixnum subseq-start subseq-end-inclusive))
               (loop for j fixnum 
                     from subseq-end-inclusive downto subseq-start do
                     (enough-elements? value-index)
                     (setf (elt sequence j) 
                           (if value-is-scalar? value (elt value value-index)))
                     (incf value-index)
                     )))))
        (t (error "Internal error. Should be caught previously!"))
        )))
    value
    ))

(defun in-order-and-exclusive? (indices-and-ranges)
  #.(optimization-declaration)
  (macrolet ((<tf (x y) `(< (the fixnum ,x) (the fixnum ,y))))
    (loop for ir on indices-and-ranges
          as i1 = (first ir)
          as i2 = (second ir)
          until (null (cdr ir))
          do
          (cond
           ((and (integerp i1) (integerp i2))
            (unless (<tf i1 i2) (return nil)))
           ((integerp i1)
            (unless (and (<tf i1 (first i2)) (<tf i1 (second i2)))
              (return nil)))
           ((integerp i2) 
            (unless (and (<tf (first i1) i2) (<tf (first i1) i2))
              (return nil)))
           (t
            (let ((i11 (first i1)) (i12 (second i1))
                  (i21 (first i2)) (i22 (second i2)))
              (declare (fixnum i11 i12 i21 i22))
              (unless (and (< i11 i21) (< i11 i22) 
                           (< i12 i21) (< i12 i22))
                (return nil)
                ))))
          finally (return t)
          )))

