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

  (defparameter *utility-iterator-user-symbols*
    '(
      create-string-odometer-iterator
      ))
    
  (defparameter *utility-iterator-api-symbols*
    (append *utility-iterator-user-symbols*
            '(
              )))

  (export *utility-iterator-api-symbols* (find-package :utils)))


#|

Listing of all iterators defined here:

  -- list-iterator: simple list (over elements); use iter-init (list) 
  -- list-on-iterator: over sublists; 
       use (iter-init (create-list-on-iterator list))
  -- general-list-iterator: over elements or sublists by 
     arbitrary step function;
       use (iter-init (create-general-list-iterator list by on?))
  -- vector-iterator: (over elements); use (iter-init vector)
  -- simple-string-iterator: (over characters); use (iter-init simple-string)
       does not work with allegro because cannot define a method on 
       simple-string type!
  -- array-iterator: (over elements in row major order); 
     use (iter-init array)
  -- hash-iterator: (over elements in system-defined order); 
     use (iter-init hash), 
       the iter-next function returns two values: key and hash-value
  -- range-iterator: (from real-valued start to or below real-valued end 
                           by real-valued step); 
       use (iter-init (create-range-iterator start end step inclusive?))
  -- string-odometer-iterator (over all strings of length N whose characters
     are limited to a provided alphabet)
  -- frame-iterator: (over slots in random order); use (iter-init frame),
       the iter-next function returns two values: slot and slot-value
       this iterator is defined in .../frames/last.lisp
|#

(define-condition iterator-error (error) 
  ((explanation :initarg :explanation :reader explanation))
  (:report
   (lambda (condition stream)
     (format stream "~A" (explanation condition))
     )))

(defun iterpec (format-string &rest format-args)
  (make-condition
   'iterator-error
   :explanation (apply 'format nil format-string format-args)
   ))
                                                     
(defstruct iterator obj state)

(defgeneric iter-init (obj)
  (:documentation 
   "Creates an iterator instance for OBJ, or NIL if no appropriate INIT method."
   ))

(defgeneric iter-next? (obj)
  (:documentation
   "Returns T if iteration is to continue, NIL if it is finished."
   ))

(defgeneric iter-next (obj)
  (:documentation
   #.(one-string-nl
      "Returns the next item in the iteration. Must be called only after a"
      "call to ITER-NEXT? which returns T."
      )))

(defmethod iter-init ((obj t)) (values obj t))

(defmethod iter-next? ((obj t))
  (error (iterpec "Don't know how to iterate over ~A, an object of type ~A"
                  obj (printed-type-of obj))))
(defmethod iter-next ((obj t)) 
  (error (iterpec "Don't know how to iterate over ~A, an object of type ~A"
                  obj (printed-type-of obj))))

;;; To create an iterator for an object, do (SETQ I (ITER-INIT object))
;;; To determine whether the iteration is terminated, call (ITER-NEXT? I)
;;; If the result is NIL the iteration has terminated.
;;; To get the values for the next iteration call (ITER-NEXT I)
;;; (More than one value is returned as a list which can then be destructured.)
;;; You must always call NEXT? and then NEXT in that order. 
;;; If NEXT? returns T you may not call NEXT? again until you have
;;; called NEXT. Once you have called NEXT you must call NEXT? before
;;; calling NEXT again and you may only then call NEXT if NEXT? returns T.

;;; Objects with predefined iterators:
;;; Lists, Vectors, Arrays (elements are returned in row major order),
;;; Hash tables, Frames, 
;;; Numeric Ranges (start, stop, step, comparison-function)

;;; Lists.  Basic iteration over a single element.

(defstruct (list-iterator (:include iterator)))

(defmethod iter-init ((obj list))
  (make-list-iterator :obj obj :state obj))
(defmethod iter-next? ((i list-iterator)) (iterator-state i))
(defmethod iter-next ((i list-iterator)) (pop (iterator-state i)))


;;; Lists.  Iteration on successive sublists (MAPLIST vs MAPCAR)

(defstruct (list-on-iterator (:include list-iterator)))

(defun create-list-on-iterator (list)
  (unless (listp list)
    (error
     (iterpec 
      (one-string-nl
       "You are creating a list iterator (using ON), but the object"
       "you are trying to iterate over, ~S, is not a list, it is" "of type ~A")
      list (printed-type-of list))))
  (make-list-on-iterator :obj list))

(defmethod iter-init ((i list-on-iterator))
  (setf (iterator-state i) (iterator-obj i))
  i)
(defmethod iter-next? ((i list-on-iterator)) (iterator-state i))
(defmethod iter-next ((i list-on-iterator)) 
  (let ((value (iterator-state i)))
    (pop (iterator-state i))
    value
    ))


;;; General list iterator, either ON or IN, with a BY function

(defstruct (general-list-iterator (:include iterator)) 
  (by 'cdr) (on? nil))

(defun create-general-list-iterator (list by on?)
  (unless (listp list)
    (error
     (iterpec 
     (one-string-nl
      "You are creating a general list iterator, but the object"
      "you are trying to iterate over, ~S, is not a list, it is" "of type ~A")
     list (printed-type-of list))))
  (make-general-list-iterator
   :obj list
   :by (canonicalize-function by)
   :on? on?
   ))

(defmethod iter-init ((i general-list-iterator))
  (setf (iterator-state i) (iterator-obj i))
  i)

(defmethod iter-next? ((i general-list-iterator))
  (iterator-state i))

(defmethod iter-next ((i general-list-iterator))
  (let ((on? (general-list-iterator-on? i)))
    (let ((value (if on? (iterator-state i) (first (iterator-state i)))))
      (setf (iterator-state i) 
            (funcall (general-list-iterator-by i) (iterator-state i)))
      value
      )))

(defun canonicalize-function (f)
  (cond
   ((symbolp f) (canonicalize-function (symbol-function f)))
   ((functionp f) f)
   ((and (list f) (eq 'quote (first f))) (canonicalize-function (first f)))
   ((and (listp f) (eq 'lambda (first f))) (compile nil f))
   (t (error
       (iterpec "Don't know how to turn ~A into a list iteration function" f)))
   ))



;;; Vectors

(defstruct (vector-iterator (:include iterator)) limit) 

(defmethod iter-init ((obj vector))
  (make-vector-iterator :obj obj :state -1 :limit (1- (length obj))))
(defmethod iter-next? ((i vector-iterator)) 
  (/= (iterator-state i) (vector-iterator-limit i)))
(defmethod iter-next ((i vector-iterator)) 
  (aref (iterator-obj i) (the fixnum (incf (the fixnum (iterator-state i))))))

;;; Strings

(defstruct (string-iterator (:include vector-iterator)))

(defmethod iter-init ((obj string))
  (make-string-iterator :obj obj :state -1 :limit (1- (length obj))))

(defmethod iter-next? ((i string-iterator)) 
  (/= (iterator-state i) (vector-iterator-limit i)))

(defmethod iter-next ((i string-iterator))
  (if (simple-string-p i) 
      (schar (the simple-string (iterator-obj i))
             (the fixnum (incf (the fixnum (iterator-state i)))))
    (char (the string (iterator-obj i))
          (the fixnum (incf (the fixnum (iterator-state i)))))
    ))

;;; BBL Strings (elements are returned as 1-length strings)

(defstruct (bblstring-iterator (:include vector-iterator)))

(defun create-bblstring-iterator (string)
  (unless (stringp string)
    (error
     (iterpec 
      (one-string-nl
       "You are creating a BBL string iterator, but the object you are"
       "trying to iterate over, ~S, is not a string, it is" "of type ~A")
      string (printed-type-of string))))
  (make-bblstring-iterator :obj string))

(defmethod iter-init ((i  bblstring-iterator))
  (setf (iterator-state i) -1)
  (setf (vector-iterator-limit i) (1- (length (iterator-obj i))))
  i)

(defmethod iter-next? ((i bblstring-iterator))
  (/= (iterator-state i) (vector-iterator-limit i)))
    
(defmethod iter-next ((i bblstring-iterator))
  (string
   (if (simple-string-p i) 
       (schar (the simple-string (iterator-obj i))
              (the fixnum (incf (the fixnum (iterator-state i)))))
     (char (the string (iterator-obj i))
           (the fixnum (incf (the fixnum (iterator-state i)))))
     )))

;;; Arrays

(defstruct (array-iterator (:include vector-iterator)))
(defmethod iter-init ((obj array))
  (make-array-iterator :obj obj :state -1 :limit (1- (array-total-size obj))))
(defmethod iter-next? ((i array-iterator)) 
  (/= (iterator-state i) (vector-iterator-limit i)))
(defmethod iter-next ((i array-iterator)) 
  (row-major-aref (iterator-obj i) (incf (iterator-state i))))


;;; Hash tables

(defstruct (hash-iterator (:include iterator)) closure key value)

#-:lispworks 
(defmethod iter-init ((obj hash-table))
  (with-hash-table-iterator (foo obj)
    (make-hash-iterator :obj obj :closure (lambda () (foo)))))
#+:lispworks
(defmethod iter-init ((obj hash-table))
  (let ((contents (hash-table-contents obj)))
    (iter-init contents)
    ))
    
(defmethod iter-next? ((i hash-iterator)) 
  (multiple-value-bind (next? key value) 
      (funcall (hash-iterator-closure i))
    (when next?
      (setf (hash-iterator-key i) key)
      (setf (hash-iterator-value i) value)
      t
      )))

(defmethod iter-next ((i hash-iterator)) 
  (list (hash-iterator-key i) (hash-iterator-value i)))

;;; Frames
;;; Defined in frames package

;;; Ranges

(defstruct (range-iterator (:include iterator)) 
  start end (step 1) (test '>))

(defstruct (fixnum-range-iterator (:include range-iterator)))

(defun create-range-iterator
       (start end step inclusive? direction &aux continuation-test)
  (ecase direction 
    (:up 
     (setq continuation-test (if inclusive? #'<= #'<))
     (when (not (plusp step))
       (when (funcall (if inclusive? '<= '<) start end)
         (error
          (iterpec
           (one-string-nl
            "Cannot iterate from ~A up to ~A ~A using steps of value ~D"
            "because the iteration would never terminate.")
           start end (if inclusive? "inclusively" "exclusively") step
           )))))
    (:down 
     (setq continuation-test (if inclusive? #'>= #'>))
     (when (not (plusp step))
       (when (funcall (if inclusive? '>= '>) start end)
         (error
          (iterpec
           (one-string-nl
            "Cannot iterate from ~A down to ~A ~A using steps of value ~D"
            "because the iteration would never terminate.")
           start end (if inclusive? "inclusively" "exclusively") step
           ))))))
  (funcall 
   (if (and (fixnump start) (fixnump end) (fixnump step))
       'make-fixnum-range-iterator 
     'make-range-iterator)
   :start start
   :end end 
   :step (ecase direction (:up step) (:down (- step))) 
   :test continuation-test
   ))

(defmethod iter-init ((obj range-iterator)) 
  (setf (iterator-obj obj) nil)
  (setf (iterator-state obj) (range-iterator-start obj))
  obj)
(defmethod iter-next? ((i range-iterator))
  (funcall (range-iterator-test i) (iterator-state i) (range-iterator-end i)))
(defmethod iter-next ((i range-iterator))
  (prog1 (iterator-state i) 
    (incf (iterator-state i) (range-iterator-step i))))

(defmethod iter-init ((obj fixnum-range-iterator)) 
  (setf (iterator-obj obj) nil)
  (setf (iterator-state obj) (range-iterator-start obj))
  obj)
(defmethod iter-next? ((i fixnum-range-iterator))
  (funcall (range-iterator-test i) (iterator-state i) (range-iterator-end i)))
(defmethod iter-next ((i fixnum-range-iterator))
  #.(optimization-declaration)
  (prog1 (iterator-state i)
    (setf (the fixnum (iterator-state i)) 
          (the fixnum (+ (the fixnum (iterator-state i)) 
                         (the fixnum (range-iterator-step i)))))))
           
;;; Steps

(defstruct (step-iterator (:include iterator)) (start nil) (step 1))

(defun create-step-iterator (start step)
  (make-step-iterator :start start :step step))
(defmethod iter-init ((obj step-iterator))
  (setf (iterator-obj obj) nil)
  (setf (iterator-state obj) (step-iterator-start obj))
  obj)
(defmethod iter-next? ((i step-iterator)) t)
(defmethod iter-next ((i step-iterator))
  (prog1 (iterator-state i) 
    (incf (iterator-state i) (step-iterator-step i))))


;;; Garrays
;;; Defined in garrays.lisp


;;; Odometer iterator

(defstruct string-odometer 
  alphabet alpha-last string-len state-vector state-string value done?)

(defun odometer-next (s slen a alast sv)
  (declare (simple-string s a))
  (declare (fixnum slen alast))
  (declare ((simple-array (unsigned-byte 16) 1) sv))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (block exit
    (loop for j fixnum from 0 below slen do
          (if (= (aref sv j) alast)
              (progn
                (setf (aref sv j) 0)
                (setf (schar s j) (aref a 0)))
            (progn
              (setf (aref sv j) (the fixnum (1+ (aref sv j))))
              (setf (schar s j) (aref a (aref sv j)))
              (return-from exit t)
              ))
          finally (return nil)
          )))

(defun create-string-odometer-iterator (alphabet string-length)
  (make-string-odometer 
   :alphabet (coerce alphabet 'simple-string)
   :string-len string-length
   :done? nil))

(defmethod iter-init ((obj string-odometer)) 
  (let ((alen (length (string-odometer-alphabet obj)))
        (slen (string-odometer-string-len obj)))
    (setf (string-odometer-alpha-last obj) (1- alen))
    (setf (string-odometer-state-vector obj)
          (make-array 
           slen :element-type '(unsigned-byte 16) :initial-element 0))
    (setf (string-odometer-state-string obj) 
          (make-string 
           slen :initial-element (char (string-odometer-alphabet obj) 0)
           )))
  obj)

(defmethod iter-next? ((obj string-odometer)) 
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (string-odometer-done? obj)
      nil
    (progn
      (setf (string-odometer-value obj) 
            (copy-seq (string-odometer-state-string obj)))
      (unless (odometer-next 
               (string-odometer-state-string obj)
               (string-odometer-string-len obj)
               (string-odometer-alphabet obj)
               (string-odometer-alpha-last obj)
               (string-odometer-state-vector obj))
        (setf (string-odometer-done? obj) t))
      t)))


(defmethod iter-next ((obj string-odometer))
  (string-odometer-value obj))
  
#||

(defun test ()
  (let ((so (create-string-odometer-iterator "acgt" 3)))
    (bioutils::xloop for s in so do (print s))))


||#