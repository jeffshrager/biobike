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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-generic-api-symbols*
    '(
      ->
      *safety*
      *suppress-warnings*
      ref
      cref
      define-ref-methods
      iterator
      iter-next?
      iter-next
      iter-init
      create-list-on-iterator
      create-bblstring-iterator
      create-general-list-iterator
      create-range-iterator
      create-step-iterator
      *subrange-safety*
      subranges
      subranges-of
      ))

  (export *utility-generic-api-symbols* (find-package :utils)))

(defconstant -> '->)

(defvar *safety* t
  #.(one-string-nl
     "Determines whether various checking is done on argument values."
     "In particular whether REF, CREF, (SETF REF) and (SETF CREF)"
     "do index-checking."
     ))

(defvar *suppress-warnings* nil
  #.(one-string-nl
     "Determines whether certain warnings are issued or not, potentially"
     "both at compile time and at run time.  In particular, used by REF"
     "when accessing slots to warn at run time when a slot does not exist."
     ))

(defgeneric ref (obj accessor &rest accessors)
  (:documentation
   #.(one-string-nl
      "Accesses a subpart of OBJ as a function of ACCESSOR and ACCESSORS."
      "A generalized AREF, GETHASH, SLOTV, etc."
      "WARNING WARNING WARNING!!!"
      "*** REF is 1-based, not 0-based."
      "*** Therefore the first element of an array A is (REF A 1), "
      "*** not (REF A 0), and similarly with respect to lists."
      "For all accessor methods except the one for multi-dimensional"
      "arrays, if there are multiple indices it is as if the REF operation"
      "was mapped over the indices; likewise, if there is a single index"
      "but it is a list, it is as if the REF operation was mapped over"
      "this list.")))

(defgeneric cref (obj accessor &rest accessors)
  (:documentation
   #.(one-string-nl
      "Accesses a subpart of OBJ as a function of ACCESSOR and ACCESSORS."
      "A generalized AREF, GETHASH, SLOTV, etc."
      "For all accessor methods except the one for multi-dimensional"
      "arrays, if there are multiple indices it is as if the REF operation"
      "was mapped over the indices; likewise, if there is a single index"
      "but it is a list, it is as if the REF operation was mapped over"
      "this list."
      )))

(defmethod ref ((obj t) index &rest indices)
  (declare (ignore index indices))
  (error 
   (one-string-nl
    "You are trying to access elements of a data object using [ ]s or REF,"
    "but the data object passed in, ~S,"
    "is not an object which has elements (like a string, list, hash table,"
    "etc.). It is an object of type ~S, which either has no elements or"
    "whose elements cannot be accessed in this way.")
   obj (printed-type-of obj)))

(defmethod cref ((obj t) index &rest indices)
  (declare (ignore index indices))
  (error 
   (one-string-nl
    "You are trying to access elements of a data object using CREF,"
    "but the object passed to CREF, ~S,"
    "is not an object which has elements (like a string, list, hash table,"
    "etc.). It is an object of type ~S, which either has no elements or"
    "whose elements cannot be accessed using CREF.")
   obj (printed-type-of obj)))

(defmacro tf1- (x) `(the fixnum (1- (the fixnum ,x))))

(defmacro tf (x) `(the fixnum ,x))

(defmacro define-ref-methods (setf? arglist &body body)
  `(progn 
     (defmethod ,(if setf? '(lisp:setf ref) 'ref) ,arglist
       (macrolet ((tf1-? (x) `(the fixnum (1- (the fixnum ,x))))
                  (rng (v args) `(apply 'subranges-of ,v ,args))
                  (setf-rng (value v args)
                    `(apply #'(lisp:setf subranges-of) ,value ,v ,args))
                  (baseref (obj index len accessor) 
                    `(vref ,obj ,index ,len ,accessor))
                  (setf-baseref (obj index new-value len accessor)
                    `(setf-vref ,obj ,index ,new-value ,len ,accessor))
                  (lbaseref (obj index) `(lref ,obj ,index))
                  (setf-lbaseref (obj index new-value) 
                    `(setf-lref ,obj ,index ,new-value)))
         (symbol-macrolet ((base 1))
           (locally ,@body)
           )))
     (defmethod ,(if setf? '(lisp:setf cref) 'cref) ,arglist
       (macrolet ((tf1-? (x) `(the fixnum ,x))
                  (rng (v args) `(apply 'subranges ,v ,args))
                  (setf-rng (value v args)
                    `(apply #'(lisp:setf subranges) ,value ,v ,args))
                  (baseref (obj index len accessor)
                    `(vcref ,obj ,index ,len ,accessor))
                  (setf-baseref (obj index new-value len accessor)
                    `(setf-vcref ,obj ,index ,new-value ,len ,accessor))
                  (lbaseref (obj index) `(lcref ,obj ,index))
                  (setf-lbaseref (obj index new-value) 
                    `(setf-lcref ,obj ,index ,new-value)))
         (symbol-macrolet ((base 0))
           (locally ,@body)
           )))))


(defmacro vref (obj index len accessor)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-vref-index ,obj ,len ,index-symbol))
       (,accessor ,obj (the fixnum (1- (the fixnum ,index-symbol))))
       )))
       
(defmacro vcref (obj index len accessor)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-vcref-index ,obj ,len ,index-symbol))
       (,accessor ,obj (the fixnum ,index-symbol))
       )))

(defmacro lref (obj index)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-lref-index ,obj ,index-symbol))
       (nth (the fixnum (1- (the fixnum ,index-symbol))) ,obj)
       )))

(defmacro lcref (obj index)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-lcref-index ,obj ,index-symbol))
       (nth (the fixnum ,index-symbol) ,obj)
       )))



(defmacro setf-vref (obj index new-value len accessor)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-vref-index ,obj ,len ,index-symbol))
       (lisp:setf 
        (,accessor ,obj (the fixnum (1- (the fixnum ,index-symbol))))
        ,new-value
        ))))

(defmacro setf-vcref (obj index new-value len accessor)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-vcref-index ,obj ,len ,index-symbol))
       (lisp:setf 
        (,accessor ,obj (the fixnum ,index-symbol))
        ,new-value
        ))))

(defmacro setf-lref (obj index new-value)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-lref-index ,obj ,index-symbol))
       (lisp:setf 
        (nth (the fixnum (1- (the fixnum ,index-symbol))) ,obj)
        ,new-value
        ))))

(defmacro setf-lcref (obj index new-value)
  (let ((index-symbol (gensym "INDEX-")))
    `(let ((,index-symbol ,index))
       #.(optimization-declaration)
       (when *safety* (verify-lcref-index ,obj ,index-symbol))
       (lisp:setf 
        (nth (the fixnum ,index-symbol) ,obj)
        ,new-value
        ))))

(defmacro range-components? (list)
  (let ((elem-symbol (gensym "ELEM-"))) 
    `(locally 
       #.(optimization-declaration)
       (loop for ,elem-symbol in ,list do 
             (when (eq ,elem-symbol '->) (return t))))))

          

;;; Vectors

(define-ref-methods nil ((obj vector) index &rest indices)
  (let ((len (length obj)))
    (declare (fixnum len))
    (flet ((vmap (indices) 
             (map
              'simple-vector
              (lambda (i) (baseref obj i len aref))
              indices
              )))
      (cond
       ((null indices) 
        (if (consp index) (vmap index) (baseref obj index len aref)))
       (t 
        (let ((all-indices (cons index indices)))
          (cond
           ((range-components? all-indices) 
            (rng obj (convert-arrows-to-ranges obj all-indices base)))
           (t (vmap all-indices))
           )))))))


;;; Strings

(defmethod ref ((obj string) index &rest indices)
  (let ((len (length obj)) (simple? (simple-string-p obj)))
    (declare (fixnum len))
    (macrolet ((rng (v args) `(apply 'subranges-of ,v ,args)))
      (flet ((string-map (indices) 
               (map 
                'simple-string
                (if simple?
                    (lambda (i) (vref obj i len schar))
                  (lambda (i) (vref obj i len char)))
                indices)))
        (if (null indices)
            ;; A single index, which might be a list of indices
            (if (consp index)
                (string-map index)
              (string 
               (if simple? 
                   (vref obj index len schar)
                 (vref obj index len char)
                 )))
          ;; Multiple indices, which might have range notation 
          (let ((all-indices (cons index indices)))
            (if (range-components? all-indices) 
                (rng obj (convert-arrows-to-ranges obj all-indices 1))
              (string-map all-indices)
              )))))))


(defmethod cref ((obj string) index &rest indices)
  (let ((len (length obj)) (simple? (simple-string-p obj)))
    (declare (fixnum len))
    (macrolet ((rng (v args) `(apply 'subranges ,v ,args)))
      (flet ((string-map (indices) 
               (map
                'simple-string
                (if simple?
                    (lambda (i) (vcref obj i len schar))
                  (lambda (i) (vcref obj i len char)))
                indices)))
        (if (null indices)
            ;; A single index, which might be a list of indices
            (if (consp index)
                (string-map index)
              (if simple?
                  (vcref obj index len schar)
                (vcref obj index len schar)
                ))
          ;; Multiple indices, which might have range notation 
          (let ((all-indices (cons index indices)))
            (if (range-components? all-indices) 
                (rng obj (convert-arrows-to-ranges obj all-indices 0))
              (string-map all-indices)
              )))))))


;;; Arrays

(define-ref-methods nil ((obj array) index &rest indices)
  (cond
   ((null indices) 
    (error "Attempt to index into array of ~D dimensions with a single index!"
           (array-rank obj)))
   ((null (cdr indices)) (aref obj (tf1-? index) (tf1-? (first indices))))
   (t (apply 'aref obj (tf1-? index) (mapcar '1- indices)))
   ))

;;; Lists

(define-ref-methods nil ((obj list) index &rest indices)
  (if (null indices) 
      (if (consp index)
          (loop for i in index collect (lbaseref obj i))
        (lbaseref obj index))
    (let ((all-indices (cons index indices)))
      (if (range-components? all-indices) 
          (rng obj (convert-arrows-to-ranges obj all-indices base))
        (extract-list-elements obj all-indices base)
        ;; (mapcar (lambda (i) (lbaseref obj i)) all-indices)
        ))))
  
;;; Hash tables

(define-ref-methods nil ((obj hash-table) key &rest keys)
  (if (null keys)
      (if (consp key)
          (loop for k in key collect (gethash k obj))
        (gethash key obj))
    (mapcar (lambda (k) (gethash k obj)) (cons key keys))
    ))

;;; Frames
;;; Defined in the frames package

;;; Garrays 
;;; Defined in garrays.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric (lisp:setf ref) (new-value obj index &rest indices))

(defgeneric (lisp:setf cref) (new-value obj index &rest indices))

(defmethod (lisp:setf ref) ((new-value t) (obj t) index &rest indices)
  (declare (ignore index indices))
  (error
   (one-string-nl
    "You are trying to set an element of a data structure using"
    "REF (or using [ ]s, which is the same thing -- perhaps something like"
    "(assign target[index] = 3).)" 
    "However, the object passed as the data structure to REF, ~S,"
    "is not an object which has elements (like a string, list, hash table,"
    "etc.), it is an object of type ~S, and you cannot use REF with it as"
    "a target.")
   obj (printed-type-of obj)))



(defmethod (lisp:setf cref) ((new-value t) (obj t) index &rest indices)
  (declare (ignore index indices))
  (error
   (one-string-nl
    "You are trying to set an element of a data structure using"
    "(SETF CREF). However, the target object passed to CREF, ~S,"
    "is not an object which has elements (like a string, list, hash table,"
    "etc.), it is an object of type ~S, and you cannot use CREF with it as"
    "a target.")
   obj (printed-type-of obj)))
  

(defun oops-not-enough-values (obj new-values indices base)
  (error 
   (one-string-nl
    "You are trying to set multiple elements of a data structure using"
    "~A, but you didn't provide enough new elements to replace"
    "the old elements.  You are trying to replace ~D elements of ~A,"
    "but you provided a sequence, ~A, which only has ~D elements."
    "There must be as many new elements as ones you want replaced!")
   (ecase base 
     (0 "(SETF CREF)")
     (1 "REF or [ ]s")
     )
   (length indices) obj new-values (length new-values)))
 

;;; Vectors

(define-ref-methods t ((new-value t) (obj vector) index &rest indices)
  (let ((len (length obj)))
    (declare (fixnum len))
    (flet ((setf-vmap (indices) 
             (if (not (typep new-value 'sequence))
                 (loop for i fixnum in indices
                       do 
                       (setf-baseref obj i new-value len aref))
               (progn 
                 (when *safety* 
                   (when (/= (length indices) (length new-value))
                     (oops-not-enough-values obj new-value indices base)))
                 (loop for i fixnum in indices 
                       for j fixnum from 0
                       do
                       (setf-baseref obj i (elt new-value j) len aref)
                       )))))
      (if (null indices)
          (if (consp index) 
              (setf-vmap index)
            (setf-baseref obj index new-value len aref))
        (let ((all-indices (cons index indices)))
          (if (range-components? all-indices)
              (setf-rng 
               new-value obj (convert-arrows-to-ranges obj all-indices base))
            (setf-vmap all-indices)
            ))))
    new-value))

;;; Strings

(defmethod (lisp:setf ref) ((new-value t) (obj string) index &rest indices)
  (let ((len (length obj)) (simple-string? (simple-string-p obj)))
    (declare (fixnum len))
    (macrolet ((setf-rng (value v args) 
                 `(apply #'(lisp:setf subranges-of) ,value ,v ,args)))
      (labels ((unstring (x i) 
                 (cond
                  ((characterp x) x) 
                  ((and (stringp x) (= 1 (length x))) (char x 0))
                  (t (error "Cannot store ~S into ~S at position ~D" x obj i))
                  ))
               (setf-string-map (indices)
                 (if (or (not (typep new-value 'sequence)) 
                         (and (stringp new-value) (= 1 (length new-value))))
                     (let ((nv (unstring new-value (first indices))))
                       (map 
                        nil 
                        (if simple-string? 
                            (lambda (i) 
                              (declare (fixnum i))
                              (setf-vref obj i nv len schar))
                          (lambda (i) 
                            (declare (fixnum i)) 
                            (setf-vref obj i nv len char)))
                        indices))
                   (progn 
                     (when *safety* 
                       (when (/= (length indices) (length new-value))
                         (oops-not-enough-values obj new-value indices 1)))
                     (let ((j 0))
                       (declare (fixnum j))
                       (map 
                        nil 
                        (if simple-string? 
                            (lambda (i) 
                              (declare (fixnum i))
                              (let ((nv (elt new-value j)))
                                (setf-vref 
                                 obj i (unstring nv i) len schar)
                                (setq j (the fixnum (1+ j)))))
                          (lambda (i) 
                            (declare (fixnum i))
                            (let ((nv (elt new-value j)))
                              (setf-vref 
                               obj i (unstring nv i) len char)
                              (setq j (the fixnum (1+ j))))))
                        indices
                        ))))))
        (if (null indices)
            (if (consp index) 
                (setf-string-map index) 
              (if simple-string? 
                  (setf-vref obj index (unstring new-value index) len schar)
                (setf-vref obj index (unstring new-value index) len char)
                ))
          (let ((all-indices (cons index indices)))
            (if (range-components? all-indices)
                (setf-rng 
                 new-value obj
                 (convert-arrows-to-ranges obj all-indices 1))
              (setf-string-map all-indices)
              )))))
    new-value))

(defmethod (lisp:setf cref) ((new-value t) (obj string) index &rest indices)
  (let ((len (length obj)) (simple-string? (simple-string-p obj)))
    (declare (fixnum len))
    (macrolet ((setf-rng (value v args) 
                 `(apply #'(lisp:setf subranges) ,value ,v ,args)))
      (labels ((setf-string-map (indices)
                 (if (not (typep new-value 'sequence))
                     (progn 
                       (verify-new-value new-value (first indices))
                       (map 
                        nil
                        (if simple-string? 
                            (lambda (i) 
                              (declare (fixnum i))
                              (setf-vcref obj i new-value len schar))
                          (lambda (i) 
                            (declare (fixnum i))
                            (setf-vcref obj i new-value len char)))
                        indices
                        ))
                   (progn 
                     (when *safety* 
                       (when (/= (length indices) (length new-value))
                         (oops-not-enough-values obj new-value indices 0)))
                     (let ((j 0))
                       (declare (fixnum j))
                       (map 
                        nil 
                        (if simple-string? 
                            (lambda (i) 
                              (declare (fixnum i))
                              (let ((nv (elt new-value j)))
                                (verify-new-value nv i)
                                (setf-vcref obj i nv len schar)
                                (setq j (the fixnum (1+ j)))))
                          (lambda (i) 
                            (declare (fixnum i))
                            (let ((nv (elt new-value j)))
                              (verify-new-value nv i)
                              (setf-vcref obj i nv len char)
                              (setq j (the fixnum (1+ j))))))
                        indices
                        )))))
               (verify-new-value (v i)
                 (unless (characterp v)
                   (error 
                    (one-string-nl
                     "Attempt to store non-character, ~S,"
                     "into a string, ~S, at position ~D.")
                    v obj i))))
        (if (null indices)
            (if (consp index)
                (setf-string-map index)
              (progn 
                (verify-new-value new-value index)
                (if simple-string? 
                    (setf-vcref obj index new-value len schar)
                  (setf-vcref obj index new-value len char)
                  )))
          (let ((all-indices (cons index indices)))
            (if (range-components? all-indices)
                (setf-rng 
                 new-value obj
                 (convert-arrows-to-ranges obj all-indices 0))
              (setf-string-map (cons index indices))
              )))))
    new-value))

;;; Arrays

(define-ref-methods t ((new-value t) (obj array) index &rest indices)
  (case (length indices)
    (0 
     (error 
      "Attempt to store into an array of ~D dimensions using a single index!"
      (array-rank obj)))
    (1 (setf (aref obj (tf1-? index) (tf1-? (first indices))) new-value))
    (2 (setf 
        (aref obj (tf1-? index) (tf1-? (first indices))
              (tf1-? (second indices))) 
        new-value))
    (3 (setf (aref 
              obj (tf1-? index) (tf1-? (first indices)) 
              (tf1-? (second indices)) (tf1-? (third indices)))
             new-value))
    (t (let ((row-major-index 
              (apply 'array-row-major-index obj 
                     (mapcar (lambda (x) (- x base)) (cons index indices)))))
         (setf (row-major-aref obj row-major-index) new-value)
         )))
  new-value
  )

;;; Lists

(define-ref-methods t ((new-value t) (obj list) index &rest indices)
  (flet ((setf-lmap (indices) 
           (if (not (typep new-value 'sequence))
               (loop for i fixnum in indices do (setf-lbaseref obj i new-value))
             (progn 
               (when *safety* 
                 (when (/= (length indices) (length new-value))
                   (oops-not-enough-values obj new-value indices base)))
               (loop for i fixnum in indices 
                     for j fixnum from 0
                     as nv = (elt new-value j)
                     do
                     (setf-lbaseref obj i nv)
                     )))))
    (if (null indices)
        (if (consp index)
            (setf-lmap index)
          (setf-lbaseref obj index new-value))
      ;; If the list is long this could be optimized by building
      ;; a hash and cdring down the list
      (let ((all-indices (cons index indices)))
        (if (range-components? all-indices)
            (setf-rng 
             new-value obj
             (convert-arrows-to-ranges obj all-indices base))
          (setf-lmap all-indices)
          ))))
  new-value)

;;; Hash tables

(define-ref-methods t ((new-value t) (obj hash-table) key &rest keys)
  (flet ((setf-hmap (keys)
           (loop for key in keys
                 for j fixnum from 0
                 do
                 (setf (gethash key obj) (elt new-value j))
                 )))
    (if (null keys)
        (if (consp key)
            (setf-hmap key)
          (setf (gethash key obj) new-value))
      (setf-hmap (cons key keys))
      ))
  new-value)

;;; Frames
;;; Defined in the frames package 

;;; Garrays 
;;; Defined in garrays.lisp


(defun verify-vref-index (obj len index)
  (declare (fixnum len))
  #.(optimization-declaration)
  (flet ((oops-bad-index (s) 
           (error 
            (one-string-nl 
             "Error accessing or modifying an element of a sequence!"
             "You are using REF (or [ ]s) to access or modify ~S,"
             s
             "All the indices provided must be positive integers less than"
             "or equal to the length of the sequence.")
            obj)))
    (when (not (integerp index))
      (oops-bad-index 
       (formatn 
        (one-string-nl 
         "using an index, ~S, which is not a positive integer,"
         "it is of type ~S.")
        index (printed-type-of index))))
    (let ((s 
           #.(one-string-nl
              "using an index, ~S, which is not an integer less than or equal"
              "to the length of the sequence you are accessing, ~D.")))
      (when (not (typep index 'fixnum))
        (oops-bad-index (formatn s index len)))
      (locally 
        (declare (fixnum index))
        (when (or (not (plusp index)) (> index len))
          (oops-bad-index (formatn s index len)) 
          )))))

(defun verify-lref-index (obj index)
  #.(optimization-declaration)
  (flet ((oops-bad-index (s) 
           (error 
            (one-string-nl 
             "Error accessing or modifying an element of a list!"
             "You are using REF (or [ ]s) to access or modify ~S,"
             s
             "All the indices provided must be positive integers"
             "greater than zero.")
            obj)))
    (when (not (integerp index))
      (oops-bad-index 
       (formatn 
        (one-string-nl 
         "using an index, ~S, which is not a positive integer,"
         "it is of type ~S.")
        index (printed-type-of index))))
    (let ((s 
           #.(one-string-nl
              "using an index, ~S, which is not an integer greater than zero."
              )))
      (when (not (typep index 'fixnum))
        (oops-bad-index (formatn s index)))
      (locally 
        (declare (fixnum index))
        (when (not (plusp index))
          (oops-bad-index (formatn s index)) 
          )))))

(defun verify-vcref-index (obj len index)
  (declare (fixnum len))
  #.(optimization-declaration)
  (flet ((oops-bad-index (s) 
           (error 
            (one-string-nl 
             "Error accessing or modifying an element of a sequence!"
             "You are using CREF to access or modify ~S,"
             s
             "All the indices provided must be non-negative integers less than"
             "the length of the sequence.")
            obj)))
    (when (not (integerp index))
      (oops-bad-index 
       (formatn 
        (one-string-nl 
         "using an index, ~S, which is not a non-negative integer,"
         "it is of type ~S.")
        index (printed-type-of index))))
    (let ((s 
           #.(one-string-nl
              "using an index, ~S, which is not a non-negative integer less"
              "than the length of the sequence you are accessing, ~D.")))
      (when (not (typep index 'fixnum))
        (oops-bad-index (formatn s index len)))
      (locally 
        (declare (fixnum index))
        (when (or (minusp index) (>= index len))
          (oops-bad-index (formatn s index len)) 
          )))))


(defun verify-lcref-index (obj index)
  #.(optimization-declaration)
  (flet ((oops-bad-index (s) 
           (error 
            (one-string-nl 
             "Error accessing or modifying an element of a list!"
             "You are using CREF to access or modify ~S,"
             s
             "All the indices provided must be non-negative integers.")
            obj)))
    (when (not (integerp index))
      (oops-bad-index 
       (formatn 
        (one-string-nl 
         "using an index, ~S, which is not a non-negative integer,"
         "it is of type ~S.")
        index (printed-type-of index))))
    (let ((s "using an index, ~S, which is not a non-negative integer."))
      (when (not (typep index 'fixnum))
        (oops-bad-index (formatn s index)))
      (locally 
        (declare (fixnum index))
        (when (minusp index)
          (oops-bad-index (formatn s index)) 
          )))))

(defun convert-arrows-to-ranges (sequence indices base)
  (when *safety* (verify-arrow-syntax indices))
  (loop with first? = t
        with rr = indices
        with results = nil
        until (null rr)
        as i1 = (first rr)
        as i2 = (second rr)
        as i3 = (third rr)
        as a1? = (eq '-> i1)
        as a2? = (eq '-> i2)
        as a3? = (eq '-> i3)
        do 
        (cond
         (first? 
          (setq first? nil)
          (cond
           ;; (-> x ...)
           ((and a1? (not a2?))
            (push (list base i2) results)
            (setq rr (cddr rr)))
           ;; (x ->)
           ((and (not a1?) a2? (null (cddr rr)))
            (macrolet ((tf (x) `(the fixnum ,x)))
              (push (list i1 (tf (+ (tf (length sequence)) (tf (1- base)))))
                    results))
            (setf rr nil))
           ;; (x -> y ...)
           ((and (not a1?) a2? (not a3?)) 
            (push (list i1 i3) results)
            (setq rr (cdddr rr)))
           ;; (x y ...)
           ((and (not a1?) (not a2?)) 
            (push i1 results)
            (setq rr (cdr rr)))
           (t (error "Illegal '->' syntax: ~S" indices))
           ))
         ;; approaching end of indices
         ((null (cddr rr))
          (cond
           ;; (x)
           ((null (cdr rr)) 
            (push i1 results)
            (setf rr nil))
           ;; (x y)
           ((and (not a1?) (not a2?)) 
            (push i1 results)
            (push i2 results)
            (setf rr nil))
           ;; (x ->)
           ((and (not a1?) a2?)
            (macrolet ((tf (x) `(the fixnum ,x)))
              (push (list i1 (tf (+ (tf (length sequence)) (tf (1- base)))))
                    results))
            (setf rr nil))
           (t (error "Illegal '->' syntax: ~S" indices))
           ))
         (t 
          (cond
           ;; (x y ...)
           ((and (not a1?) (not a2?)) 
            (push i1 results)
            (setf rr (cdr rr)))
           ;; (x -> y ...)
           ((and (not a1?) a2? (not a3?))
            (push (list i1 i3) results)
            (setf rr (cdddr rr)))
           (t (error "Illegal '->' syntax: ~S" indices))
           )))
        finally (return (nreverse results))
        ))
            
           

(defun verify-arrow-syntax (indices)
  #.(optimization-declaration)
  (flet ((oops (text) 
           (error (formatn "Illegal range syntax: ~S,~%~A~%" indices text))))
    (loop 
      with first? = t
      with rr = indices
      until (null rr)
      as i1 = (first rr)
      as i2 = (second rr)
      as i3 = (third rr)
      as a1? = (eq '-> i1)
      as a2? = (eq '-> i2)
      as a3? = (eq '-> i3)
      do 
      (cond
       (first?
        (setq first? nil)
        (cond
         ;; (->)
         ((not i2) (oops "a '->' by itself is illegal!"))
         ;; (-> x ...)
         ((and a1? (not a2?)) (setf rr (cddr rr)))
         ;; (x -> y ...)
         ;; (x ->)
         ((and (not a1?) a2? (not a3?)) (setf rr (cdddr rr)))
         ;; (x y ...)
         ((and (not a1?) (not a2?)) (setf rr (cdr rr)))
         ;; (-> -> ...)
         ((and a1? a2?) (oops "two '->' in a row!"))
         ;; (x -> -> ...)
         ((and (not a1?) a2? a3?) (oops "two '->' in a row!"))
         ))
       ((null (cdr rr))
        (when a1? 
          (oops
           (one-string-nl
            "'->' occurs as last index while the previous index was used"
            "as a component of a prior range."
            )))
        (setq rr nil))
       ((null (cddr rr))
        (cond
         ;; (x y)
         ((and (not a1?) (not a2?)) (setf rr nil))
         ;; (x ->)
         ((and (not a1?) a2?) (setf rr nil))
         ;; (-> ->)
         ((and a1? a2?) (oops "two '->' in a row!"))
         ;; (-> x)
         ((and a1? (not a2?))
          (oops 
           (one-string-nl
            "'->' occurs as the second to last index, while the previous"
            "index was used a a component of a prior range."
            )))))
       (t
        (cond
         ;; (x y ...)
         ((and (not a1?) (not a2?)) (setf rr (cdr rr)))
         ;; (x -> y ...)
         ((and (not a1?) a2? (not a3?)) (setf rr (cdddr rr)))
         ;; (x -> -> ...)
         ((and (not a1?) a2? a3?) (oops "two '->' in a row!"))
         ;; (-> -> ...) 
         ((and a1? a2?) (oops "two '->' in a row!"))
         ;; (-> x ...)
         ((and a1? (not a2?))
          (oops 
           (one-string-nl
            "'->' occurs in a position immediately after the occurance"
            "of another index used as a component of a prior range."
            )))))))))

#|

(defun t1 (n)
  (declare (fixnum n))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x "abcdefghijkl")
        (indices (mapcar '1+ (iota 10))))
    (time 
     (loop for j fixnum from 0 below n do 
           (setf (ref x indices) "mnopqrstuv")))))

(defun t2 (n)
  (declare (fixnum n))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x "abcdefghijkl")
        (indices (mapcar '1+ (iota 10))))
    (let ((*safety* nil))
      (time 
       (loop for j fixnum from 0 below n do 
             (setf (ref x indices) "mnopqrstuv"))))))
    
(defun t3 (n)
  (declare (fixnum n))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x "abcdefghijkl")
        (y "mnopqrstuv")
        (indices (iota 10)))
    (declare (simple-string x y))
    (time 
     (loop for j fixnum from 0 below n
           do
           (loop for i fixnum in indices
                 for k fixnum from 0 below 10
                 do 
                 (setf (aref x i) (aref y k))
                 )))
    (list x y)))
                       
|#