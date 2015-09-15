;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utilities; -*-

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

;;; Author:  JP Massar 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *garray-api-symbols*
    '(
      gref
      make-garray
      defgarray
      describe-garray
      pprint-garray
      garray-rank
      garray-axes
      garray-name
      garray-named
      set-garray-name
      garray-axis-extent
      garray-axis-first-index
      garray-axis-last-index
      garray-data
      garray-adjustable?
      garray-potential-total-size
      garray-current-total-size
      gmap
      gmapset
      garray-component-indices
      garray-component-garray
      garray-copy
      copy-garray
      garray-convert-hash-index
      *garray-default-value*
      )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export *garray-api-symbols* :utils))

(defparameter *garray-default-value* :<garray-not-set-marker>
  "This should never be SETQ'ed, only dynamically bound via LET and friends!")

(defconstant *garray-accessor-error-marker* :<garray-access-error-marker>)

(defparameter *garray-not-filled-in-yet-marker* :<not-filled-in-yet!>)

(defvar *current-garray*)
(defvar *current-indices*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GARRAY and AXIS data structures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass garray ()
  ((rank :accessor garray-rank :initarg :rank)
   (axes :accessor garray-axes :initarg :axes)
   (data :accessor garray-data :initarg :data)
   ;; a garray is by default adjustable.
   (adjustable?
    :accessor garray-adjustable?
    :initform t
    :initarg :adjustable?
    )
   ;; See MAKE-GARRAY for a discussion of this slot.
   (if-accessed-location-not-set
    :accessor garray-if-accessed-location-not-set
    :initarg :if-accessed-location-not-set
    )
   (if-accessed-location-does-not-exist
    :accessor garray-if-accessed-location-does-not-exist
    :initarg :if-accessed-location-does-not-exist
    )
   (create-arms?
    :accessor garray-create-arms? 
    :initarg :create-arms?
    )
   (plist :accessor garray-plist :initarg :plist :initform nil)
   (named :accessor garray-named :initarg :named :initform nil)
   ))

;;; The AXIS DESCRIPTOR object. 
 ;;; Different kinds of axes have different slots.

(defclass axis () ()) 

(defclass num-axis (axis)
  ((extent :accessor axis-extent :initarg :extent))) 

(defclass xa-axis (num-axis)
  ((min :accessor axis-min :initarg :min)
   (limit :accessor axis-limit :initarg :limit)
  ))

(defclass enum-axis (num-axis)
  ((possibles :accessor axis-possibles :initarg :possibles)
   (test :accessor axis-test :initform 'eql :initarg :test)
   (hash :accessor axis-hash :initarg :hash)
   )) 

(defclass hash-axis (axis)
  ((test :accessor axis-test :initform 'equal :initarg :test)))

;; For XA axes, compute and set the AXIS-EXTENT slot.

;; For ENUM axes, set up the hash table to map from possibles to indices, 
;; and set the AXIS-EXTENT slot.

(defmethod initialize-instance :after ((axis xa-axis) &rest args)
  (declare (ignore args))
  (setf (axis-extent axis) (- (axis-limit axis) (axis-min axis))))

(defmethod initialize-instance :after ((axis enum-axis) &rest args)
  (declare (ignore args))
  (let ((hash (make-hash-table :test (axis-test axis))))
    (setf (axis-hash axis) hash)
    (loop for j fixnum from 0 for possible in (axis-possibles axis) do
          (setf (gethash possible hash) j))
    (setf (axis-extent axis) (hash-table-count hash))
    ))

;;; Convert a garray index into a Lisp array index.

(defgeneric axis-true-index (axis index)
  (:documentation
   "Convert a user-level index into an internal index into a Lisp vector"
   ))

(defmethod axis-true-index ((axis xa-axis) (index t)) 
  (the fixnum (- (the fixnum index) (the fixnum (axis-min axis)))))
(defmethod axis-true-index ((axis enum-axis) (index t))
  (gethash index (axis-hash axis)))
(defmethod axis-true-index ((axis hash-axis) (index t)) index)

(defgeneric index-in-axis-bounds (axis index)
  (:documentation
   "Return T is INDEX is a currntly valid index wrt AXIS, otherwise NIL"
   ))

(defmethod index-in-axis-bounds ((axis xa-axis) (index t))
  (and (>= index (axis-min axis)) (< index (axis-limit axis))))
(defmethod index-in-axis-bounds ((axis enum-axis) (index t))
  (integerp (gethash index (axis-hash axis))))
(defmethod index-in-axis-bounds ((axis hash-axis) (index t))
  (let ((real-index (garray-convert-hash-index index (axis-test axis))))
    (multiple-value-bind (value key-present?)
        (gethash real-index (axis-hash axis))
      (declare (ignore value))
      key-present?
      )))

(defun axis-numeric? (x) (typep x 'num-axis)) 
(defun axis-arrayish? (x) (typep x 'xa-axis))
(defun axis-enum? (x) (typep x 'enum-axis)) 
(defun axis-hash? (x) (typep x 'hash-axis))
(defun axis-generic? (x) (typep x 'hash-axis))

(defgeneric garray-copy-axis (axis)
  (:documentation 
   #.(one-string-nl
      "Returns a copy of AXIS, a new instance. For numeric axes,"
      "the MIN and MAX can be modified independently.  (Other kinds of axes"
      "have no slots that can be modified once created.)"
      )))

(defmethod garray-copy-axis ((axis xa-axis))
  (make-instance 'xa-axis :min (axis-min axis) :limit (axis-limit axis)))
  
(defmethod garray-copy-axis ((axis enum-axis))
  (make-instance 
   'enum-axis 
   ;; No need to copy possibles here as list is immutable (in theory).
   :possibles (axis-possibles axis)
   :test (axis-test axis)
   ;; No need to copy hash table as it is immutable (in theory).
   :hash (axis-hash axis)
   ))

(defmethod garray-copy-axis ((axis hash-axis))
  (make-instance 'hash-axis :test (axis-test axis)))

(defun garray-name (g) (garray-named g)) 
(defun set-garray-name (g name) (setf (garray-named g) name))

(defun garray-autoload-function (g) (getf (garray-plist g) :autoload-function))
(defun set-garray-autoload-function (g f)
  (setf (getf (garray-plist g) :autoload-function) f))

(define-condition garray-never-set (error) 
  ((data :initarg :data :reader garray-never-set-data))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Ruh Roh.  Garray ~A element with indices ~S never set...~%"
      (first (garray-never-set-data condition))
      (rest (garray-never-set-data condition))
      ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GARRAY creation and initialization.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-garray
       (axis-descriptors
        &key
        (initial-element *garray-default-value* initial-element-provided?)
        (adjustable t)
        (if-accessed-location-not-set :error)
        (if-accessed-location-does-not-exist :error)
        (create-arms? t)
        (named nil)
        )
  #.(one-string-nl
     "Create a generalized array from a set of axis descriptors."
     "If NAMED is not nil, it must be a symbol or a string.  The name"
     "of the symbol or the string is used as the name of the garray."
     "If ADJUSTABLE is T (the default), any numeric-range axes are adjustable,"
     "that is, attempting to store a value into a location out of the range"
     "of the axis will cause the garray to 'expand' and the axis to be"
     "modified, rather than signalling an error." 
     "(ENUM axes are never adjustable, and HASH axes are always adjustable.)"
     "If INITIAL-ELEMENT is provided it is used to initialize the elemental"
     "locations of the garray, if any (a garray which has a hash axis has no"
     "elemental locations initially).  Providing an INITIAL-ELEMENT overrides"
     "any value of IF-ACCESSED-LOCATION-NOT-SET (see below)."
     "IF-ACCESSED-LOCATION-NOT-SET can be:"
     "  -- :ERROR, in which case an error is signalled if any attempt is made"
     "to access a location in this garray which has never been set."
     "  -- :DEFAULT, in which case the value of *GARRAY-DEFAULT-VALUE* at the"
     "time of the call the MAKE-GARRAY will be returned whenever a location"
     "which has never been set is accessed."
     "  -- Anything else, in which case that value will be returned whenver a"
     "location has which has never been set is accessed."
     "IF-ACCESSED-LOCATION-DOES-NOT-EXIST can be:"
     " -- :ERROR, in which case an error is signalled if an attempt is made to"
     "use an invalid numeric or enum index."
     " -- Anything else, in which case that value is returned when such an"
     "attempt is made."
     "If CREATE-ARMS? is NIL (the default is T), only"
     "the internal data structure for the first axis is created; subsidiary"
     "internal structures are created only when needed.  If it is non-NIL,"
     "then all the data structures for all the non-hash axes prior to the"
     "first hash axis are created, along with empty hash tables for the"
     "first hash axis.  This is intended to converse space for large garrays"
     "but must be used with care, as parts of the garray will simply not"
     "exist, and functions like GMAP and GMAPSET will ignore these"
     "non-existent arms of the garray."
     )
  (declare (ignore element-type))
  (when initial-element-provided?
    (setq if-accessed-location-not-set initial-element))
  (let ((axes (parse-axis-declarations axis-descriptors)))
    (setq if-accessed-location-not-set
          (cond
           ((eq if-accessed-location-not-set :error)
            *garray-accessor-error-marker*)
           ((eq if-accessed-location-not-set :default)
            *garray-default-value*)
           (t if-accessed-location-not-set)
           ))
    (when named 
      (when (symbolp named) (setq named (string named)))
      (unless (stringp named) (error "Name must be a string!")))
    (let ((garray 
           (create-garray-structure 
            axes create-arms? adjustable if-accessed-location-not-set
            if-accessed-location-does-not-exist named
            )))
      garray
      )))

(defun recreate-garray 
       (axes index-value-ntuples 
             &key 
             (adjustable t)
             (if-accessed-location-not-set :error)
             (if-accessed-location-does-not-exist :error)
             (create-arms? t)
             (named nil))
  (let ((garray 
         (create-garray-structure 
          axes create-arms? adjustable if-accessed-location-not-set
          if-accessed-location-does-not-exist named
          )))
    (loop for index-value-ntuple in index-value-ntuples 
          do
          (apply-setf-gref 
           garray (first index-value-ntuple) (rest index-value-ntuple)))
    garray
    ))

(defun create-garray-structure 
       (axes create-arms? adjustable if-accessed-location-not-set
             if-accessed-location-does-not-exist named)
  (let* ((garray (create-garray-from-axes axes))
         (*current-garray* garray))
    (setf (garray-create-arms? garray) create-arms?)
    (setf (garray-adjustable? garray) adjustable)
    (setf (garray-if-accessed-location-not-set garray) 
          if-accessed-location-not-set)
    (setf (garray-if-accessed-location-does-not-exist garray)
          if-accessed-location-does-not-exist)
    (setf (garray-named garray) named)
    (initialize-garray-structures garray create-arms?)
    garray
    ))
  



(defmacro defgarray (name axis-descriptors &rest make-garray-keyargs)
  #.(one-string-nl
     "Define a global variable initialized as a generalized array."
     "the AXIS-DESCRIPTORS argument is not evaluated."
     "See (MAKE-GARRAY ...) for allowable keywords and values.")
  (declare (ignore adjustable? if-accessed-location-not-set))
  (declare (ignore create-all-possible-array-structures?))
  (when (not (listp axis-descriptors))
    (setq axis-descriptors (list axis-descriptors)))
  `(defvar ,name (make-garray ',axis-descriptors ,@make-garray-keyargs))
  )

;;; Given a set of AXIS DESCRIPTORS, create a garray defined by those axes.  
;;; This is trivial as all we do is create the garray structure and provide
;;; its rank and axes.  Later routines provide its internal structure
;;; and other properties.

(defun create-garray-from-axes (axes)
  (make-instance 'garray :rank (length axes) :axes axes))
   
(defun initialize-garray-structures (garray create-all?)
  (let ((axes (garray-axes garray))
        (rank (garray-rank garray))
        )
    ;; Create the toplevel structure
    (setf (garray-data garray)
          (create-data-structure-for-axis
           (first axes) (garray-adjustable? garray)
           ))
    ;; Initialize the toplevel structure
    (cond
     ((= rank 1) 
      (initialize-last-axis 
       (first axes)
       (garray-data garray)
       (garray-if-accessed-location-not-set garray)
       ))
     (t (initialize-other-axis (first axes) (garray-data garray)))
     )
    ;; Create and initialize the substructures as possible if requested.
    (when create-all? (garray-initialize-arms-through-first-hash garray))
    ))

(defun garray-initialize-arms-through-first-hash (garray)
  ;; Start the recursion with the toplevel data structure
  ;; and all but the first axis.
  (garray-initialize-arms-through-first-hash-aux
   (garray-data garray)
   (rest (garray-axes garray))
   ))

(defun garray-initialize-arms-through-first-hash-aux (data remaining-axes)
  ;; We're done if there are no more axes...
  (unless (null remaining-axes)
    (cond
     ;; This can only happen if the toplevel structure is a hash,
     ;; since we've already created the toplevel structure.  So just return.
     ((hash-table-p data) nil)
     ;; We're almost through if the next axis is a hash... We just need to
     ;; create the empty hash tables in the DATA structure.
     ((axis-hash? (first remaining-axes))
      (create-and-initialize-subaxis data remaining-axes nil))
     ;; Otherwise the next axis must be an array axis, so recurse
     (t (create-and-initialize-subaxis data remaining-axes t))
     )))
  
(defun create-and-initialize-subaxis (data remaining-axes recurse?)
  (let ((axis (first remaining-axes))
        (last-axis? (null (cdr remaining-axes)))
        (adjustable? (garray-adjustable? *current-garray*))
        (default (garray-if-accessed-location-not-set *current-garray*))
        )
    (when (hash-table-p data)
      (ierror "Should not be initializing hash tables!!"))
    (loop for j fixnum from 0 below (length data) 
          as substructure = (create-data-structure-for-axis axis adjustable?)
          do
          (setf (aref data j) substructure)
          (if last-axis?
              (initialize-last-axis axis substructure default)
            (progn
              (initialize-other-axis axis substructure)
              (when recurse?
                (garray-initialize-arms-through-first-hash-aux
                 substructure (rest remaining-axes)
                 )))))))

(defmethod create-data-structure-for-axis ((axis num-axis) (adjustable? t))
  (make-array (list (axis-extent axis)) :adjustable adjustable?))

(defmethod create-data-structure-for-axis ((axis hash-axis) (adjustable? t))
  (make-hash-table :test (axis-test axis)))

(defmethod initialize-last-axis ((axis num-axis) (data t) (default t))
  (fill data default))

(defmethod initialize-last-axis ((axis hash-axis) (data t) (default t))
  nil)

(defmethod initialize-other-axis ((axis num-axis) (data t))
  (fill data *garray-not-filled-in-yet-marker*))

(defmethod initialize-other-axis ((axis hash-axis) (data t))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GARRAY element reference (GREF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *non-existent-arm-access?*)

(defun gref (garray &rest indices)
  #.(one-string-nl
     "Retrieve a value from a generalized array."
     "If no value exists in the location indicatd by INDICES an error may"
     "be signalled (if the GARRAY so indicates, see MAKE-GARRAY, the default"
     "is to signal an error), or a default value may be returned (defined"
     "when the array is created, see MAKE-GARRAY and *GARRAY-DEFAULT-VALUE*)."
     "Two values are returned -- the value in the indicated position or a"
     "default value, and a boolean which is T iff an attempt was made to"
     "access an arm of the garray which does not yet exist."
     "Also (setf (gref ...) value) is an allowable form with the obvious"
     "semantics.")
  (if (typep garray 'garray)
      (progn
        (unless (= (length indices) (garray-rank garray))
          (garray-oops-wrong-number-of-indices 'gref garray indices))
        (let ((*current-garray* garray) 
              (*current-indices* indices)
              (*non-existent-arm-access?* nil)
              )
          (if (null (garray-autoload-function garray))
              (values (gref-aux garray indices) *non-existent-arm-access?*)
            (handler-case 
                (values (gref-aux garray indices) *non-existent-arm-access?*)
              (garray-never-set
               ()
               (apply (garray-autoload-function garray) garray indices)
               (handler-case 
                   (values (gref-aux garray indices) *non-existent-arm-access?*)
                 (garray-never-set () (error "Autoload failed!"))
                 ))))
          ))
    (error "Not a GARRAY: ~A" garray)
    ))

(defun garray-oops-wrong-number-of-indices (f garray indices)
  (error "~A: Garray ~A has ~D dimensions but ~D indices were used."
         f garray (garray-rank garray) (length indices)))

;;; Recurse down the axes until we arrive
;;; at the last axis or we determine that the place the element being asked
;;; for is supposedly located at has never been stored into.

(defun gref-aux (garray indices)
  ;; Start the recursion 
  (gref-aux-internal 
   garray (garray-data garray) (garray-axes garray) indices 0))

(defun gref-aux-internal (garray data axes indices axis-depth)
  (multiple-value-bind (ok? not-ok-return-value)
      (verify-single-axis-index garray (first axes) (first indices) axis-depth)
    (when (not ok?) (return-from gref-aux-internal not-ok-return-value))
    (if (null (cdr indices))
        ;; This is the last axis
        (gref-aux-last-axis (first axes) (first indices) data)
      ;; An axis other than the last axis
      (progn
        (let ((next-level 
               (gref-aux-other-axis (first axes) (first indices) data)
               ))
          ;; No data structure created there yet...
          (if (eq next-level *garray-not-filled-in-yet-marker*)
              (let ((val 
                     (garray-if-accessed-location-not-set *current-garray*)))
                (and (eq val *garray-accessor-error-marker*) (oops-never-set))
                (setq *non-existent-arm-access?* t)
                val
                )
            ;; Keep going...
            (gref-aux-internal 
             garray next-level (cdr axes) (cdr indices) (1+ axis-depth))
            ))))))

(defgeneric gref-aux-last-axis (axis index data)
  (:documentation
   #.(one-string-nl
      "Returns the value indexed by INDEX in DATA. INDEX's meaning depends"
      "on the type of AXIS -- either a numeric index or a hash key."
      "The value returned can be either:"
      "  -- the default for the garray, if no value has previously been"
      "     placed at this index."
      "  -- a value previously placed at this location."
      )))

(defmethod gref-aux-last-axis ((axis num-axis) index data)
  (let ((result (aref data (axis-true-index axis index))))
    (and (eq result *garray-accessor-error-marker*) (oops-never-set))    
    result
    ))

(defmethod gref-aux-last-axis ((axis hash-axis) index data)
  (let ((real-index (garray-convert-hash-index index (axis-test axis))))
    (multiple-value-bind (value present?)
        (gethash real-index data)
      (if present? 
          value
        (let ((val (garray-if-accessed-location-not-set *current-garray*)))
          (and (eq val *garray-accessor-error-marker*) (oops-never-set))
          val
          )))))
         
(defgeneric gref-aux-other-axis (axis index data)
  (:documentation
   #.(one-string-nl
      "Returns the next level data structure (a vector or a hash table,"
      "depending on the AXIS type) stored at INDEX in DATA."
      "The value returned may also be *GARRAY-NOT-FILLED-IN-YET-MARKER*"
      "indicating that this 'arm' of the garray has not yet been created."
      )))

(defmethod gref-aux-other-axis ((axis num-axis) index data)
  (let ((result (aref data (axis-true-index axis index))))
    (unless (or (vectorp result)
                (hash-table-p result)
                (eq result *garray-not-filled-in-yet-marker*))
      (ierror "Bad value in non-final garray vector element. ~A" result)
      )
    result
    ))
      
(defmethod gref-aux-other-axis ((axis hash-axis) index data)
  (let ((real-index (garray-convert-hash-index index (axis-test axis))))
    (multiple-value-bind (value present?)
        (gethash real-index data)
      (if present? value *garray-not-filled-in-yet-marker*)
      )))

(defun oops-never-set ()
  (error 
   (make-condition 
    'garray-never-set 
    :data (cons *current-garray* *current-indices*)
    )))

(defun verify-single-axis-index (garray axis index axis-depth)
  ;; Hash indices are always valid in this context
  ;; A hash index that doesn't exist gets handled higher up.
  (flet ((out-of-range ()
           (let ((rval (garray-if-accessed-location-does-not-exist garray)))
             (if (eq :error rval)
                 (error 
                  (one-string-nl
                   "Out of range index, ~A, for axis ~D ~A,"
                   "in call to GREF using garray ~A")
                  index axis-depth (print-garray-axis-readably axis nil) garray
                  )
               (return-from verify-single-axis-index
                 (values nil rval))))))
    (etypecase axis
      (xa-axis
       (unless (integerp index)
         (error 
          "Non-integer index, ~A, for axis ~S (~A), in call to GREF for ~A"
          index axis-depth axis garray
          ))
       (unless (index-in-axis-bounds axis index) (out-of-range)))
      (enum-axis
       (unless (index-in-axis-bounds axis index) (out-of-range)))
      (hash-axis nil)
      ))
  t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GARRAY element storage (SETF GREF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun setf-gref (garray value &rest indices)
  #.(one-string-nl
     "Store VALUE into a generalized array GARRAY at the position designated"
     "by INDICES.")
  (if (typep garray 'garray)
      (progn
        (unless (= (length indices) (garray-rank garray))
          (garray-oops-wrong-number-of-indices 
           '(setf gref) garray indices))
        (let ((*current-garray* garray) (*current-indices* indices))
          (setf-gref-aux garray value indices)
          ))
    (error "Not a GARRAY: ~A" garray)
    ))

(defun setf-gref-aux (garray value indices)
  ;; Start the recursion
  (setf-gref-aux-internal
   (garray-data garray)
   (garray-axes garray)
   indices
   value
   0
   ))

(defun setf-gref-aux-internal (data axes indices value axis-depth)
  #+debug
  (print
   (list 'aux-internal 'data data 'axes axes 'indices indices
         'value value 'axis-depth axis-depth))
  (let ((first-axis (first axes)) (first-index (first indices)))
    ;; Is the index within the current access bounds?
    (unless (or (axis-generic? first-axis)
                (index-in-axis-bounds first-axis first-index))
      ;; No -- is the garray adjustable?
      (unless (garray-adjustable? *current-garray*)
        (oops-not-adjustable 
         first-axis first-index axis-depth
         ))
      ;; It is adjustable.  Try to adjust this axis' bounds.
      ;; This will error out unless the adjustment attempt is successful.
      ;; This puts the new axis into *current-garray* in the right
      ;; location, but we have to make sure FIRST-AXIS gets the new value.
      (let ((new-axis
             (adjust-axis-size-or-error-out 
              first-axis first-index axis-depth
              )))
        (setq first-axis new-axis)
        ))
    (if (null (cdr indices))
        (progn
          #+debug
          (print 'setting-last-axis)
          (setf-gref-aux-last-axis first-axis first-index data value))
      ;; On downward to the next level -- does the data structure exist?
      (let ((next-level (gref-aux-other-axis first-axis first-index data)))
        (when (eq next-level *garray-not-filled-in-yet-marker*)
          ;; No, create the data structure.
          #+debug
          (print 'creating-next-level)
          (setq 
           next-level
           (create-next-level-at-index 
            data first-axis (second axes) (first indices))))
        ;; Keep going down
        #+debug
        (print (list 'next-level next-level))
        (setf-gref-aux-internal 
         next-level (cdr axes) (cdr indices) value (1+ axis-depth)
         )))))


(defun oops-not-adjustable (axis index axis-index)
  (error 
   (one-string-nl
    "The garray ~A is not adjustable,"
    "but you are trying to store data into a non-existent location"
    "of that garray."
    "You are using an index of ~A for the ~:R axis, but the allowable"
    "indices ~A ~S inclusive.")
   *current-garray* 
   index
   (1+ axis-index)
   (etypecase axis
     (xa-axis "are in the range ")
     (enum-axis "are "))
   (etypecase axis
     (xa-axis (formatn "(~D .. ~D)" (axis-min axis) (1- (axis-limit axis))))
     (enum-axis (axis-possibles axis))
     )))

(defun create-next-level-at-index (data data-axis next-axis index)
  ;; Create the new structure based on AXIS
  (let* ((garray *current-garray*)
         (new-structure
          (create-data-structure-for-axis 
           next-axis (garray-adjustable? garray)))
        (default (garray-if-accessed-location-not-set garray))
        )
    ;; Store it into DATA at INDEX
    (store-new-data-structure-at-index data index data-axis new-structure)
    ;; Initialize it appropriately using DEFAULT
    (if (eq next-axis (lastelem (garray-axes garray)))
        (initialize-last-axis next-axis new-structure default)
      (initialize-other-axis next-axis new-structure))
    new-structure
    ))

(defmethod store-new-data-structure-at-index 
           ((data array) index axis new-structure)
  (setf (aref data (axis-true-index axis index)) new-structure))

(defmethod store-new-data-structure-at-index 
           ((data hash-table) index axis new-structure)
  ;; (declare (ignorable axis))
  (let ((real-index (garray-convert-hash-index index (axis-test axis))))
    (setf (gethash real-index data) new-structure)))
  
(defgeneric setf-gref-aux-last-axis (axis index data value)
  (:documentation
   #.(one-string-nl
      "Put VALUE into the data structure DATA at position INDEX."
      "DATA is either a vector or a hash table depending on AXIS."
      )))

(defmethod setf-gref-aux-last-axis ((axis num-axis) index data value)
  (setf (aref data (axis-true-index axis index)) value))

(defmethod setf-gref-aux-last-axis ((axis hash-axis) index data value)
  (let ((real-index (garray-convert-hash-index index (axis-test axis))))
    ;; (test-for-valid-key-for-hash-test index (axis-test axis))
    (setf (gethash real-index data) value)))

(defun adjust-axis-size-or-error-out (axis index axis-depth)
  ;; Create the new axis structure
  (let ((new-axis (create-adjusted-axis-or-barf axis index axis-depth)))
    ;; Replace the old axis structure with the new one in the garray
    (setf (nth axis-depth (garray-axes *current-garray*)) new-axis)
    ;; Adjust data structures built using the old axis specification
    ;; to conform to the new axis specification.
    (adjust-garray-arms-for-new-axis new-axis axis axis-depth)
    new-axis
    ))
      
;; An XA-AXIS simply gets extended as necessary in the proper direction.
(defmethod create-adjusted-axis-or-barf ((axis xa-axis) index axis-depth)
  (declare (ignorable axis-depth))
  (let ((min (axis-min axis)) (limit (axis-limit axis)))
    (cond
     ((< index min) (make-instance 'xa-axis :min index :limit limit))
     ((>= index limit) (make-instance 'xa-axis :min min :limit (1+ index)))
     (t (ierror "Index is within range of AXIS.  Should not be here!"))
     )))

(defmethod create-adjusted-axis-or-barf ((axis enum-axis) index axis-depth)
  (error 
   (formatn
    (one-string-nl
     "Cannot extend or modify an axis defined by an enumerated set."
     "The index ~S is not a member of the enumeration defined for axis ~D (~A)"
     "for garray ~A.")
     index axis-depth axis *current-garray*
     )))
  
(defmethod create-adjusted-axis-or-barf ((axis hash-axis) index axis-depth) 
  (declare (ignorable index axis-depth))
  (ierror "Should never get here."))
  
(defun adjust-garray-arms-for-new-axis (new-axis old-axis axis-index)
  (let ((garray *current-garray*))
    (if (zerop axis-index)
        ;; We are adjusting the first axis of the garray.
        (adjust-garray-toplevel new-axis old-axis)
      ;; We are adjusting some other axis of the garray
      (adjust-garray-arms-for-new-axis-aux
       (garray-data garray) (garray-axes garray) new-axis old-axis
       ))))

(defun adjust-garray-toplevel (new-axis old-axis)
  (let* ((garray *current-garray*)
         (initial-value
          (if (= 1 (garray-rank garray))
              (garray-if-accessed-location-not-set garray)
            *garray-not-filled-in-yet-marker*
            )))
    ;; Make the lisp array holding the data bigger using ADJUST-ARRAY
    (setf (garray-data garray)
          (make-data-structure-conform-to-new-axis 
           new-axis (garray-data garray) initial-value
           ))
    ;; Shift the data in the array up or down as appropriate
    (make-data-in-extended-structure-conform-to-new-limits
     new-axis old-axis (garray-data garray) initial-value
     )))

(defun adjust-garray-arms-for-new-axis-aux (data axes new-axis old-axis)
  (if (not (eq new-axis (second axes)))
      ;; Recurse into the garray along the first remaining axis
      ;; until we find the axis we want to modify
      (descend-into-garray-arm-for-new-axis
       (first axes)  data axes new-axis old-axis)
    ;; The axis below the first remaining one is the axis we need
    ;; to modify the data for.
    (let* ((garray *current-garray*)
           (initial-value
            (if (eq new-axis (lastelem axes))
                (garray-if-accessed-location-not-set garray)
              *garray-not-filled-in-yet-marker*
              )))
      (adjust-garray-component-for-new-axis
       (first axes) data new-axis old-axis initial-value
       ))))

(defmethod descend-into-garray-arm-for-new-axis 
           ((first-remaining-axis num-axis) data axes new-axis old-axis)
  (loop for j from 0 below (axis-extent first-remaining-axis) 
        as value = (aref data j) do
        (unless (eq value *garray-not-filled-in-yet-marker*)
          (adjust-garray-arms-for-new-axis-aux
           value (cdr axes) new-axis old-axis
           ))))

(defmethod descend-into-garray-arm-for-new-axis 
           ((first-remaining-axis hash-axis) data axes new-axis old-axis)
  (maphash
   (lambda (key value)
     (declare (ignore key))
     (adjust-garray-arms-for-new-axis-aux
      value (cdr axes) new-axis old-axis
      ))
   data
   ))

(defmethod adjust-garray-component-for-new-axis
           ((first-remaining-axis num-axis) data new-axis old-axis init)
  (loop for j from 0 below (axis-extent first-remaining-axis) 
        as value = (aref data j) do
        (unless (eq value *garray-not-filled-in-yet-marker*)
          (setf (aref data j)
                (make-data-structure-conform-to-new-axis 
                 new-axis value init
                 ))
          (make-data-in-extended-structure-conform-to-new-limits
           new-axis old-axis value init
           ))))

(defmethod adjust-garray-component-for-new-axis
           ((first-remaining-axis hash-axis) data new-axis old-axis init)
  (maphash
   (lambda (key value)
     (setf (gethash key data)
           (make-data-structure-conform-to-new-axis 
            new-axis value init
            ))
     (make-data-in-extended-structure-conform-to-new-limits
      new-axis old-axis value init
      ))
   data
   ))

(defun insure-adjustable (data)
  (unless (vectorp data)
    (ierror "Data structure to adjust is not a vector!"))
  (unless (adjustable-array-p data)
    (ierror "Data structure to adjust is not an adjustable array!")))

(defmethod make-data-structure-conform-to-new-axis
           ((axis xa-axis) data initial-element)
  (insure-adjustable data)
  (adjust-array data (list (axis-extent axis)) :initial-element initial-element)
  )

(defmethod make-data-structure-conform-to-new-axis
           ((axis enum-axis) data initial-element)
  (declare (ignorable data initial-element))
  (ierror "Should not be able to get here. Should have errored previously.")
  )

(defmethod make-data-structure-conform-to-new-axis
           ((axis hash-axis) data initial-element)
  (declare (ignorable initial-element))
  data
  )

(defmethod make-data-in-extended-structure-conform-to-new-limits
           ((new-axis xa-axis) (old-axis xa-axis) data initial-value)
  (let ((new-min (axis-min new-axis))
        (old-min (axis-min old-axis))
        (new-limit (axis-limit new-axis))
        (old-limit (axis-limit old-axis))
        )
    ;; (print (list 'new-min new-min 'om old-min 'nl new-limit 'ol old-limit))
    ;; (print (list 'data (map 'list 'identity data)))
    (cond
     ;; The maximum must have increased, initialize all the new elements
     ;; above and including the old limit.  
     ((= new-min old-min)
      (loop for j from old-limit below new-limit do
            (setf (aref data (- j old-min)) initial-value)
            ))
     ;; The minimum must have shifted down
     ((= new-limit old-limit)
      ;; How much did it shift?
      (let ((offset (- old-min new-min)))
        (shift-data-up-by-offset-and-initialize data offset initial-value)
        ))
     (t (ierror "How could both min and limit change??"))
     )))

(defmethod make-data-in-extended-structure-conform-to-new-limits
           ((new-axis t) (old-axis enum-axis) data initial-value)
  (declare (ignorable data initial-value))
  (ierror "Should not be able to get here."))

(defmethod make-data-in-extended-structure-conform-to-new-limits
           ((new-axis t) (old-axis hash-axis) data initial-value)
  (declare (ignorable data initial-value))
  nil)

(defun shift-data-up-by-offset-and-initialize (data offset initial-value)
  (unless (zerop offset)
    (loop for j fixnum from (1- (length data)) downto offset do
          (setf (aref data j) (aref data (- j offset))))
    (loop for j fixnum from 0 below offset do
          (setf (aref data j) initial-value)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Valid declarations
;;;  -- a single integer (if positive, range is (0 <integer>), if negative,
;;;     range is (<integer> 1)
;;;  -- a range (min limit)
;;;  -- a '$' or the keyword :hash (the default hash test is EQUAL)
;;;  -- a two-element list ($ <hash-test>) or (:hash <hash-test>)
;;;  -- a list that begins with :enum (the default test is EQL)
;;;  -- a list that begins with ((:enum <test>) ...)
;;;  -- a random list, treated as (cons :enum <random-list>)

(defun parse-axis-declaration (ad)
  "Returns an object of type (class) AXIS or errors if AD is not valid"
  (labels ((oops (reason)
             (error "GARRAY axis invalid: ~A~%  Reason: ~A" ad reason))
           (like (x string)
             (and (or (symbolp x) (stringp x))
                  (string-equal string (string x))))
           (hashp? (x) (or (like x "$") (like x "HASH")))
           (hash-test (x)
             (cond
              ((or (eq x 'eq) (eq x 'eql) (eq x 'equal) (eq x 'equalp)) x)
              ((or (eq x 'string=) (eq x #'string=)) 'equal)
              ((or (eq x 'string-equal) (eq x #'string-equal)) 'equalp)
              ((eq x #'eq) 'eq)
              ((eq x #'eql) 'eql)
              ((eq x #'equal) 'equal)
              ((eq x #'equalp) 'equalp)
              ((stringp x)
               (cond
                ((find-symbol (string-upcase x) :common-lisp)
                 (hash-test (intern (string-upcase x) :common-lisp)))
                (t (hash-test (intern x *package*)))
                ))
              ((symbolp x)
               (if (keywordp x)
                   (let ((f (find-symbol (string x) :common-lisp)))
                     (and f (fboundp f) f)
                     (oops "Unrecognized hash test")
                     )
                 (progn
                   #-:allegro
                   (warn "Using non-portable hash table predicate ~S" x)
                   x
                   )))
              (t (oops "Unrecognized hash test"))
              )))
    (cond
     ((integerp ad)
      (if (plusp ad)
          (make-instance 'xa-axis :min 0 :limit ad)
        (make-instance 'xa-axis :min ad :limit 1)))
     ((hashp? ad)
      (make-instance 'hash-axis :test 'equal))
     ((not (listp ad)) (oops "Unknown atomic axis designator"))
     ((and (= 2 (length ad)) (integerp (first ad)) (integerp (second ad)))
      (unless (< (first ad) (second ad))
        (oops "Invalid range (first value must be less than second value)"))
      (make-instance 'xa-axis :min (first ad) :limit (second ad)))
     ((and (= 2 (length ad)) 
           (hashp? (first ad))
           (let ((test (hash-test (second ad))))
             (and test (make-instance 'hash-axis :test test))
             )))
     ((like (first ad) "ENUM")
      (when (zerop (length ad)) (oops "Zero length ENUM axis not allowed"))
      (make-instance 'enum-axis :possibles (cdr ad)))
     ((and (listp (first ad)) (like (caar ad) "ENUM"))
      (cond
       ((zerop (length (cdr ad))) (oops "Zero length ENUM axis not allowed"))
       ((not (= 2 (length (first ad)))) (oops "Bad ENUM hash-type specifier"))
       ((not (hash-test (cadar ad))) (oops "Bad ENUM hash test specifier"))
       (t 
        (make-instance
         'enum-axis :possibles (cdr ad) :test (hash-test (cadar ad))
         ))))
     (t (parse-axis-declaration (cons :enum ad)))
     )))

(defun parse-axis-declarations (ads) (mapcar 'parse-axis-declaration ads))

;;; If you understand this, you're a better Lisper than me.
;;; Thanks to Peter Seibel to providing the template.

(define-setf-expander gref (x &rest indices &environment env)
  (multiple-value-bind (xtemps xvals xstore-vars xwriter xreader)
      (get-setf-expansion x env)
    (let ((store (gensym))
          (xstore (first xstore-vars))
          (index-gensyms
           (loop for j from 0 for i in indices collect
                 (progn i (gensym (format nil "INDEX~D-" j))))))
      (if (rest xstore-vars) (error "Too many store vars."))
      (values
       index-gensyms
       indices
       (list store)
       `(let*
            (,@(mapcar #'list xtemps xvals)
             (,xstore 
              (if (ignore-errors (typep ,xreader 'garray))
                  ,xreader
                (create-garray-from-&rest-indices ,@index-gensyms))))
          (setf-gref ,xstore ,store ,@index-gensyms)
          ,xwriter
          ,store)
       `(gref ,xreader ,@index-gensyms)))))


(defun create-garray-from-&rest-indices (&rest indices)
  (make-garray (mapcar 'axis-descriptor-from-index indices)))

(defun axis-descriptor-from-index (index)
  (cond
   ((integerp index) (if (minusp index) index (1+ index)))
   (t '$)
   ))
    

