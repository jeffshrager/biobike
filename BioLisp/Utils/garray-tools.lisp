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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun funcall-gref (g &rest indices) (apply-gref g indices))

(defun apply-gref (g indices) 
  (let ((len (length indices)))
    (case len
      (1 (gref g (first indices)))
      (2 (gref g (first indices) (second indices)))
      (3 (gref g (first indices) (second indices) (third indices)))
      (4 (gref g (first indices) (second indices) (third indices) 
               (fourth indices)))
      (5 (gref g (first indices) (second indices) (third indices) 
               (fourth indices) (fifth indices)))
      (otherwise 
       (error 
        (one-string-nl
         "System error.  APPLY-GREF can't handle garrays with more than 5 axes."
         "Please report this to the system administrators."
         ))))))

(defun funcall-setf-gref (g value &rest indices) 
  (apply-setf-gref g value indices))

(defun apply-setf-gref (g value indices)
  (let ((len (length indices)))
    (case len
      (1 (setf (gref g (first indices)) value))
      (2 (setf (gref g (first indices) (second indices)) value))
      (3 (setf (gref g (first indices) (second indices) (third indices)) value))
      (4 (setf (gref g (first indices) (second indices) (third indices) 
                     (fourth indices)) value))
      (5 (setf (gref g (first indices) (second indices) (third indices) 
                     (fourth indices) (fifth indices)) value))
      (otherwise 
       (error 
        (one-string-nl
         "System error.  APPLY-SETF-GREF can't handle garrays with more"
         "than 5 axes.  Please report this to the system administrators."
         ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-garray-axis-readably (axis stream))

(defgeneric garray-axis-to-descriptor (axis))

(set-dispatch-macro-character #\# #\G 'pound-g-garray-reader)

(defmethod print-object ((g garray) stream)
  "Print a garray unreadably with rank and axis type info"
  (if *print-readably* 
      (print-garray-readably g stream)
    (progn
      (format 
       stream
       "<Garray ~A~Dd ("
       (let ((n (garray-named g)))
         (if n (formatn "named ~A " n) "")) 
       (garray-rank g))
      (print-garray-contents g stream)
      (format stream ")>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code dealing with printing a garray so that it can be read back in.


(defun print-garray-readably (g stream)
  (format stream "#G(")
  (print-garray-axes-readably g stream)
  (princ #\Space stream)
  (print-garray-parameters-readably g stream)
  (princ #\Space stream)
  (print-garray-indices-and-contents-readably g stream)
  (format stream ") ")
  )

(defun print-garray-axes-readably (g stream)
  (format stream "(")
  (loop for axis in (garray-axes g) for j from 0 do
        (print-garray-axis-readably axis stream))
  (format stream ")")
  )

(defmethod print-garray-axis-readably ((axis num-axis) stream)
  (format stream "~S" (list 'num-axis :extent (axis-extent axis))))

(defmethod print-garray-axis-readably ((axis xa-axis) stream)
  (format
   stream "~S" 
   (list 'xa-axis :min (axis-min axis) :limit (axis-limit axis))))

(defmethod print-garray-axis-readably ((axis enum-axis) stream)
  (format
   stream "~S" 
   (list 'enum-axis :possibles (axis-possibles axis) :test (axis-test axis))))

(defmethod print-garray-axis-readably ((axis hash-axis) stream)
  (format
   stream "~S" 
   (list 'hash-axis :test (axis-test axis))))

(defun print-garray-parameters-readably (g stream)
  (format 
   stream "~S" 
   (list 
    :adjustable? (garray-adjustable? g)
    :if-accessed-location-not-set (garray-if-accessed-location-not-set g)
    :if-accessed-location-does-not-exist 
    (garray-if-accessed-location-does-not-exist g)
    :create-arms? (garray-create-arms? g)
    :named (garray-named g)
    )))
        

(defun print-garray-indices-and-contents-readably (g stream)
  "Prints out the indices of an element of a garray and the element."
  (let* ((*current-garray* g)
         (data (garray-data g))
         (axes (garray-axes g))
         )
    (format stream "(")
    (print-garray-indices-and-contents-readably-aux
     0 (first axes) data (rest axes) nil stream)
    (format stream ")")
    )) 

(defun print-garray-indices-and-contents-readably-aux
       (dimension axis data remaining-axes indices s)
  (let ((last-axis? (null remaining-axes)))
    (loop for i in (garray-possible-indices axis data) 
          as indices-stack = (cons i indices)
          as current-indices = (reverse indices-stack)
          do
          (if last-axis?
              (let ((*current-indices* current-indices))
                (format 
                 s "~S "
                 (list 
                  current-indices
                  (handler-case
                      (gref-aux-last-axis axis i data)
                    (error () *garray-accessor-error-marker*)
                    ))))
            (let ((substructure (gref-aux-other-axis axis i data)))
              (when (eq substructure *garray-not-filled-in-yet-marker*)
                (error "Cannot write out garray with some axis not filled in!"))
              (print-garray-indices-and-contents-readably-aux 
               (1+ dimension) (first remaining-axes)
               substructure (rest remaining-axes)
               indices-stack s
               ))))))

(defun pound-g-garray-reader (stream char arg)
  (declare (ignore char arg))
  (destructuring-bind (axes-data parameter-data contents)
      (read stream t)
    (let* ((axis-descriptors 
            (mapcar 
             'garray-axis-to-descriptor 
             (mapcar 
              (lambda (axis-data) (apply 'make-instance axis-data))
              axes-data
              )))
           (garray 
            (make-garray 
             axis-descriptors
             :adjustable 
             (getf parameter-data :adjustable?)
             :if-accessed-location-not-set 
             (getf parameter-data :if-accessed-location-not-set)
             :if-accessed-location-does-not-exist
             (getf parameter-data :if-accessed-location-does-not-exist)
             :create-arms? 
             (getf parameter-data :create-arms?)
             :named
             (getf parameter-data :named)
             )))
      (loop for (indices content) in contents do 
            (apply 'setf-gref garray content indices))
      garray
      )))
            
(defmethod garray-axis-to-descriptor ((axis num-axis))
  (axis-extent axis))

(defmethod garray-axis-to-descriptor ((axis xa-axis))
  (list (axis-min axis) (axis-limit axis)))

(defmethod garray-axis-to-descriptor ((axis enum-axis))
  (list (list :enum (axis-test axis))
        (axis-possibles axis)
        ))

(defmethod garray-axis-to-descriptor ((axis hash-axis))
  (list '$ (axis-test axis)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code dealing with creating a form that will recreate a garray when executed.

(defvar *garray-element-code-generator* nil)

(defvar *index-data-pairs* nil)

(defun default-code-element-generator-function ()
  (if (find-package :vpl)
      (symbol-of-package "SERIALIZED-SNIPPET-VALUE" :vpl)
    'default-garray-creation-form-code-element-generator
    ))

(defun default-garray-creation-form-code-element-generator (x)
  (cond
   ((symbolp x) 
    (if (or (eq x t) (eq x nil) (keywordp x))
        x 
      `(quote ,x)))
   ((listp x) `(quote ,x))
   (t x)
   ))

(defun garray-index-data-pairs 
       (g &optional (f (default-code-element-generator-function)))
  (let ((*garray-element-code-generator* f))
    (mapcar 'eval (create-garray-index-data-pairs g))
    ))

(defun create-garray-creation-form 
       (g &optional (f (default-code-element-generator-function)))
  (let ((*garray-element-code-generator* f))
    `(recreate-garray
      (list ,@(create-garray-axes-creation-forms g))
      (list ,@(create-garray-index-data-pairs g))
      :adjustable ,(garray-adjustable? g)
      :create-arms? ,(garray-create-arms? g)
      :if-accessed-location-not-set 
      ,(quote-if-necessary (garray-if-accessed-location-not-set g))
      :if-accessed-location-does-not-exist 
      ,(garray-if-accessed-location-does-not-exist g)
      )))

(defun quote-if-necessary (x)
  (cond
   ((or (and (symbolp x) (not (constantp x)))
        (consp x))
    `(quote ,x))
   (t x)
   ))

(defun create-garray-axes-creation-forms (g)
  (loop for axis in (garray-axes g) collect 
        (create-garray-axis-creation-form axis)))

(defmethod create-garray-axis-creation-form ((axis t))
  (error "Should not have garray axis of type ~S" (type-of axis)))

(defmethod create-garray-axis-creation-form ((axis xa-axis))
  `(make-instance 'xa-axis :min ,(axis-min axis) :limit ,(axis-limit axis)))

(defmethod create-garray-axis-creation-form ((axis hash-axis))
  `(make-instance 'hash-axis :test ',(axis-test axis)))

(defmethod create-garray-axis-creation-form ((axis enum-axis))
  `(make-instance 
    'enum-axis 
    :possibles 
    (list ,@(mapcar *garray-element-code-generator* (axis-possibles axis)))
    :test ',(axis-test axis)
    ))
  
(defun create-garray-index-data-pairs (g)
  (let ((*current-garray* g)
        (data (garray-data g))
        (axes (garray-axes g))
        (*index-data-pairs* nil))
    (create-garray-index-data-pairs-aux 
     g 0 (first axes) data (rest axes) nil)
    (mapcar 
     (lambda (x) `(list ,@x))
     *index-data-pairs*
     )))

(defun create-garray-index-data-pairs-aux 
       (g dimension axis data remaining-axes indices)
  (let ((last-axis? (null remaining-axes))
        (bad-value nil)
        (bad-index nil)
        (success? nil))
    (setq 
     success?
     (block bad-value 
       (loop
        for i in (garray-possible-indices axis data) 
        as indices-stack = (cons i indices)
        as current-indices = (reverse indices-stack)
        do
        (if last-axis?
            (block no-value
              (let ((*current-indices* current-indices))
                (setq 
                 current-indices 
                 (loop for i in current-indices collect 
                       (handler-case 
                           (funcall *garray-element-code-generator* i)
                         (error 
                          ()
                          (setq bad-index i)
                          (return-from bad-value nil)
                          ))))
                (push 
                 (cons 
                  ;; If the value doesn't exist, simply don't 
                  ;; generate any code for it.
                  (handler-case
                      (let ((value (gref-aux-last-axis axis i data)))
                        ;; But if we can't generate code for a real
                        ;; value, give it up.
                        (handler-case 
                            (funcall *garray-element-code-generator* value)
                          (error 
                           ()
                           (setq bad-value value) 
                           (return-from bad-value nil)
                           )))
                    (error () (return-from no-value)))
                  current-indices)
                 *index-data-pairs*
                 )))
          (let ((substructure (gref-aux-other-axis axis i data)))
            (when (eq substructure *garray-not-filled-in-yet-marker*)
              (error 
               "Cannot create garray code with some axis not filled in!"))
            (create-garray-index-data-pairs-aux  
             g (1+ dimension) (first remaining-axes)
             substructure (rest remaining-axes)
             indices-stack 
             )))
        finally (return t)
        )))
    (unless success? 
      (cond 
       (bad-value 
        (error 
         (one-string-nl
          "Cannot create garray code because a value,"
          "~A,"
          "cannot be turned into code using the function ~A.")
         bad-value 
         *garray-element-code-generator*
         ))
       (bad-index 
        (error 
         (one-string-nl
          "Cannot create garray code because one of the indices to the garray,"
          "~A,"
          "cannot be turned into code using the function ~A.")
         bad-index
         *garray-element-code-generator*
         ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code that prints out information about a garray, not in rereadable form


(defun print-garray-contents (g stream)
  (loop for axes on (garray-axes g) for j from 0 do
        (format stream "~A~A"
                (let ((axis-type (type-of (first axes))))
                  (when (eq axis-type 'xa-axis) 
                    (setq axis-type 'numeric-axis))
                  (reverse
                   (subseq (reverse (string-capitalize (string axis-type))) 5)
                   ))
                (if (rest axes) "," "")
                )))

(defun describe-garray 
       (g &key 
          (stream *standard-output*)
          (type "GARRAY")
          (name "Unnamed")
          (details? t)
          (header? t)
          &aux (p stream)
          )
  "Displays the properties and axes of a generalized array."
  (when header? 
    (vif (gname (garray-name g))
         (format p "Description of ~A named ~A~%" type (string-upcase gname))
         (if (string-equal name "Unnamed")
             (format p "Description of ~A ~A~%" name type)
           (format p "Description of ~A ~A~%" type name)
           )))
  (format p "  Rank: ~D~%" (garray-rank g))
  (format p "  Adjustable: ~A~%" (garray-adjustable? g))
  (when details? 
    (format p "  If accessed location exists but has never been set:  ~S~%"
            (let ((v (garray-if-accessed-location-not-set g)))
              (if (eq v *garray-accessor-error-marker*)
                  :error-is-signaled 
                v
                )))
    (format p "  If accessed location does not exist: ~S~%"
            (garray-if-accessed-location-does-not-exist g))
    (format p "  Plist : ~S ~%" (garray-plist g)))
  (loop for axis in (garray-axes g) for j from 0 do
        (format p "  Axis ~D: " j)
        (if details? 
            (pprint-axis axis type p)
          (mini-pprint-axis axis p)
          )
        (terpri p)
        ))
          

(defmethod pprint-axis ((a xa-axis) type stream)
  (format stream "<~A axis type: ~A, min: ~D, limit: ~D, extent: ~D> "
           type (type-of a) (axis-min a) (axis-limit a) (axis-extent a)))
(defmethod mini-pprint-axis ((a xa-axis) stream)
  (format stream "Type: numeric, min: ~D, limit: ~D, extent: ~D"
          (axis-min a) (axis-limit a) (axis-extent a)))

(defmethod pprint-axis ((a enum-axis) type stream)
  (format stream "<~A axis type: ~A, number: ~D, test: ~A, possibles: ~S> "
          type (type-of a) (axis-extent a) (axis-test a) (axis-possibles a)))
(defmethod mini-pprint-axis ((a enum-axis) stream)
  (format stream "Type: enumeration, number of possibilities: ~D"
           (axis-extent a)))
(defmethod pprint-axis ((a hash-axis) type stream)
  (format stream "<~A axis type: ~A, test: ~A> "
          type (type-of a) (axis-test a)))
(defmethod mini-pprint-axis ((a hash-axis) stream)
  (format stream "Type: Hash, test: ~A"
          (axis-test a)))

(defun pprint-garray (g &optional (p *standard-output*) (header? t))
  "Prints out the elements of a garray, one per line, indented."
  (let* ((*current-garray* g)
        (data (garray-data g))
        (axes (garray-axes g))
        )
    (when header? (format p "~&~%Contents of generalized array ~S~%~%" g))
    (pprint-garray-aux 0 (first axes) data (rest axes) nil 1 p)
    ))

(defun pprint-garray-aux (dimension axis data remaining-axes indices indent s)
  (let ((last-axis? (null remaining-axes)))
    (loop for i in (garray-possible-indices axis data) 
          as indices-stack = (cons i indices)
          as current-indices = (reverse indices-stack)
          do
          (dotimes (j indent) (write-char #\Space))
          (if last-axis?
              (let ((*current-indices* current-indices))
                (format 
                 s "~S : ~S~%" 
                 current-indices
                 (handler-case
                     (gref-aux-last-axis axis i data)
                   (error () *garray-accessor-error-marker*)
                   )))
            (let ((substructure (gref-aux-other-axis axis i data)))
              (if (eq substructure *garray-not-filled-in-yet-marker*)
                  (format s "~S : *~%" current-indices)
                (format s "~S :~%" current-indices))
              (pprint-garray-aux 
               (1+ dimension) (first remaining-axes)
               substructure (rest remaining-axes)
               indices-stack (+ indent 2)
               s
               ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; MAPPING AND ITERATION ON GARRAYS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *gmap-missing-action*)
(defvar *gmap-missing-value*)
(defvar *gmap-arm-missing-action*)
(defvar *gmap-arm-missing-value*)
(defvar *gmap-some-arm-not-present?*)

(defun gmap 
       (f garray 
          &key
          (flatten? nil)
          (arm-missing-action :missing-marker)
          (arm-missing-value *garray-not-filled-in-yet-marker*)
          (missing-action :missing-value)
          (missing-value (garray-if-accessed-location-not-set garray))
          )
  #.(one-string-nl 
     "Map a function F over every element of GARRAY and return a nested"
     "list of results.  The nesting is as deep as the rank of GARRAY."
     "By default, if any part of the garray substructure does not yet exist"
     "*GARRAY-NOT-FILLED-IN-YET-MARKER* is returned in place of the list that"
     "would otherwise be returned at that nesting level.  Instead, if"
     "ARM-MISSING-ACTION is :error, an error is signalled if any substructure"
     "is missing.  If ARM-MISSING-VALUE is provided, that value is used."
     "By default, if any existing location has never been set, the function"
     "is called with the default value the GARRAY was initialized with."
     "Instead, if MISSING-ACTION is :error, an error is signalled.  If"
     "MISSING-VALUE is provided, that value is used instead of the default"
     "value.")
  (let ((data (garray-data garray))
        (axes (garray-axes garray))
        (*current-garray* garray)
        (*gmap-some-arm-not-present?* nil)
        (*gmap-missing-action* missing-action)
        (*gmap-missing-value* missing-value)
        (*gmap-arm-missing-action* arm-missing-action)
        (*gmap-arm-missing-value* arm-missing-value)
        )
    (let ((nested-list (gmap-garray data axes f)))
      (values 
       (if flatten? (flatten nested-list) nested-list)
       *gmap-some-arm-not-present?*
       ))))

(defun gmap-garray (data axes f)
  (cond
   ;; We're down to the last axis.  Get a list of all the results.
   ((null (cdr axes))
    (gmap-garray-final-axis (first axes) data f))
   ;; Descend into the garray structure along this axis, collecting results.
   (t 
    (gmap-garray-other-axis (first axes) data (cdr axes) f)
    )))

(defmethod gmap-garray-final-axis ((axis num-axis) data f)
  (let* ((gmap-default *gmap-missing-value*)
         (missing-action *gmap-missing-action*)
         (error-if-not-set? (eq missing-action :error))
         (garray-default 
          (garray-if-accessed-location-not-set *current-garray*)
          ))
    (loop for j fixnum from 0 below (axis-extent axis) 
          as value = (aref data j) collect
          (if (and error-if-not-set? (eq value garray-default))
              (error 
               "GMAP acessing position in garray ~A that has never been set."
               *current-garray*
               )
            (funcall f (if (eq value garray-default) gmap-default value))
            ))))

(defmethod gmap-garray-final-axis ((axis hash-axis) data f)
  (lmaphash 
   (lambda (key value) (declare (ignore key)) (funcall f value))
   data
   ))

(defmethod gmap-garray-other-axis ((axis num-axis) data rest-of-axes f)
  (let* ((action *gmap-arm-missing-action*)
         (error-if-missing? (eq action :error))
         (missing-marker *garray-not-filled-in-yet-marker*)
         (gmap-arm-missing-value *gmap-arm-missing-value*)
         )
    (loop for j fixnum from 0 below (axis-extent axis) 
        as value = (aref data j) 
        collect
        (if (not (eq value missing-marker))
            (gmap-garray value rest-of-axes f)
          (progn
            (setq *gmap-some-arm-not-present?* t)
            (if error-if-missing?
                (error "GMAP accessing non-existent garray substructure.")
              gmap-arm-missing-value
              ))))))

(defmethod gmap-garray-other-axis ((axis hash-axis) data rest-of-axes f)
  (loop for value being the hash-values of data collect
        (gmap-garray value rest-of-axes f)
        ))


(defun gmapset 
       (f garray 
          &key
          (arm-missing-action :ignore)
          (missing-action :missing-value)
          (missing-value (garray-if-accessed-location-not-set garray))
          )
  #.(one-string-nl
   "Map F over each element and store result back into that element's slot"
   "GARRAY is side-effected and returned."
   "If an existing location has never been set then MISSING-ACTION determines"
   "what happens.  If MISSING-ACTION is :error, an error is signalled."
   "If it is :missing-value (the default), then the function F is called"
   "with the default value recorded when GARRAY was created and the result"
   "stored as usual.  If MISSING-ACTION is :ignore the function F is not"
   "called and no value is stored in the location."
   "If part of the garray substructure does not exist, ARM-MISSING-ACTION"
   "determines what happens.  If ARM-MISSING-ACTION is :ignore (the default)"
   "the missing substructure is ignored.  Otherwise, if it is :error, an"
   "error is signalled.  No other options are permissible at this time.")
  (unless (member missing-action '(:error :ignore :missing-value))
    (error "GMAPSET invalid MISSING-ACTION value: ~A" missing-action))
  (unless (member arm-missing-action '(:ignore :error))
    (error "GMAPSET invalid ARM-MISSING-ACTION value: ~A" arm-missing-action))
  (let ((data (garray-data garray))
        (axes (garray-axes garray))
        (*current-garray* garray)
        (*gmap-some-arm-not-present?* nil)
        (*gmap-arm-missing-action* arm-missing-action)
        (*gmap-missing-action* missing-action)
        (*gmap-missing-value* missing-value)
        )
    (gmapset-garray data axes f)
    (values garray *gmap-some-arm-not-present?*)
    ))

(defun gmapset-garray (data remaining-axes f)
  (let ((next-axis (first remaining-axes))
        (other-axes (cdr remaining-axes)))
    (cond
     ((null other-axes)
      (gmapset-garray-final-axis next-axis data f))
     (t
      (gmapset-garray-other-axis next-axis data other-axes f)
      ))))

(defmethod gmapset-garray-final-axis ((axis num-axis) data f)
  (let ((action *gmap-missing-action*)
        (default (garray-if-accessed-location-not-set *current-garray*))
        )
    (loop for j fixnum from 0 below (axis-extent axis)
          as value = (aref data j) do
          (if (eq value default)
              (ecase action
                ((:missing-value :default) 
                 (setf (aref data j) (funcall f value)))
                (:ignore nil)
                (:error
                 (error 
                  "GMAPSET on garray ~A has non-previously set location"
                  *current-garray*
                  )))
            (setf (aref data j) (funcall f value))
            ))))
          

(defmethod gmapset-garray-final-axis ((axis hash-axis) data f)
  (maphash
   (lambda (key value) (setf (gethash key data) (funcall f value)))
   data
   ))

(defmethod gmapset-garray-other-axis ((axis num-axis) data other-axes f)
  (let ((action *gmap-arm-missing-action*)
        (marker *garray-not-filled-in-yet-marker*)
        )
    (loop for j fixnum from 0 below (axis-extent axis)
          as value = (aref data j) do
          (if (eql value marker)
              (ecase action
                (:ignore nil)
                (:error
                 (error
                  "GMAPSET on garray ~A encountered non-existent arm."
                  *current-garray*
                  )))
            ;; Recurse into the structure.
            (gmapset-garray value other-axes f)
            ))))

(defmethod gmapset-garray-other-axis ((axis hash-axis) data other-axes f)
  (maphash
   (lambda (key value)
     (declare (ignore key))
     (gmapset-garray value other-axes f))
   data
   ))
   

(defun garray-axis-extent (garray axis-index)
  "Returns size of the AXIS-INDEX'th axis, or NIL"
  (let ((axis (nth axis-index (garray-axes garray))))
    (etypecase axis
      (hash-axis nil)
      (num-axis (axis-extent axis))
      )))

(defun garray-axis-first-index (garray axis-index)
  "Returns smallest (first) index along the AXIS-INDEX dimension, or NIL"
  (let ((axis (nth axis-index (garray-axes garray))))
    ;; order is critical since XA and ENUM axes are subtypes of NUM-AXIS
    (etypecase axis
      (hash-axis nil)
      (xa-axis (axis-min axis))
      (enum-axis (first (axis-possibles axis)))
      (num-axis 0)
      )))

(defun garray-axis-last-index (garray axis-index)
  "Returns largest (last) index along the AXIS-INDEX dimension, or NIL"
  (let ((axis (nth axis-index (garray-axes garray))))
    ;; order is critical since XA and ENUM axes are subtypes of NUM-AXIS
    (etypecase axis
      (hash-axis nil)
      (xa-axis (1- (+ (axis-min axis) (axis-extent axis))))
      (enum-axis (lastelem (axis-possibles axis)))
      (num-axis (1- (axis-extent axis)))
      )))


;;; data-TOTAL-SIZE

(defun garray-current-total-size (garray &key (missing-action :count))
  #.(one-string-nl
     "Return number of elements currently in GARRAY.  If a location has"
     "not been set yet, MISSING-ACTION determines whether it is counted."
     "If MISSING-ACTION is :count (the default), it is counted."
     "If MISSING-ACTION is :ignore, it is not counted."
     "If MISSING-ACTION is :error, an error is signalled.")
  (unless (member missing-action '(:count :ignore :error))
    (error "Invalid MISSING-ACTIION value for GARRAY-CURRENT-TOTAL-SIZE."))
  (let ((data (garray-data garray))
        (axes (garray-axes garray))
        (*current-garray* garray)
        )
    (garray-current-total-size-aux 
     (first axes) data (rest axes) missing-action
     )))

(defmethod garray-current-total-size-aux 
           ((axis num-axis) data other-axes missing-action)
  ;; If this is the last axis, return number of elements in data structure
  (if (null other-axes)
      (ecase missing-action
        (:count (axis-extent axis))
        (:ignore
         (let ((default (garray-if-accessed-location-not-set *current-garray*)))
           (loop for x across data sum (if (eq x default) 0 1))
           ))
        (:error
         (let ((default (garray-if-accessed-location-not-set *current-garray*)))
           (when (some (lambda (x) (eq x default)) data)
             (error "GARRAY-CURRENT-TOTAL-SIZE: Encountered non-set location")
             ))
         (axis-extent axis)
         ))
    ;; Otherwise sum up number of elements in substructures, ignoring
    ;; substructures that don't exist.
    (loop for x across data sum
          (if (eq x *garray-not-filled-in-yet-marker*)
              0
            (garray-current-total-size-aux
             (first other-axes) x (rest other-axes) missing-action
             )))))

(defmethod garray-current-total-size-aux
           ((axis hash-axis) data other-axes missing-action)
  (if (null other-axes)
      (hash-table-count data)
    (let ((sum 0) (next-axis (first other-axes)) (axes (rest other-axes)))
      (maphash
       (lambda (key value) 
         (declare (ignore key))
         (incf
          sum
          (garray-current-total-size-aux next-axis value axes missing-action)
          ))
       data)
      sum
      )))


(defun garray-potential-total-size (garray)
  #.(one-string-nl
     "Returns :infinite if garray has a hash table axis, otherwise"
     "the product of the extent of all the axes (i.e., its total capacity.")
  (if (some (lambda (x) (axis-hash? x)) (garray-axes garray))
      :infinite
    (apply '* (mapcar 'axis-extent (garray-axes garray)))
    ))
    

;;; *** Warning: This currently returns the converted indices, not
;;; the original indices. *** 

(defun garray-component-indices (garray &rest indices)
  #.(one-string-nl
     "Returns the set of valid keys for the component of GARRAY specified"
     "by INDICES.  The number of INDICES must be less than the rank of"
     "GARRAY, and may be zero (producing the valid keys for the toplevel"
     "component.")
  (let ((len (length indices))
        (rank (garray-rank garray))
        (axes (garray-axes garray))
        )
    (unless (< len rank)
      (error 
       (one-string
        "Invalid number of indices (~D) to GARRAY-COMPONENT-INDICES. "
        "The number of indices must be less than the rank of the garray (~S)."
        len rank
        )))
    (if (null indices)
        (garray-possible-indices (first axes) (garray-data garray))
      (garray-component-indices-aux (garray-data garray) axes indices)
      )))

(defun garray-component-indices-aux (data axes indices)
  (let ((substructure 
         (gref-aux-other-axis (first axes) (first indices) data)
         ))
    (when (eq substructure *garray-not-filled-in-yet-marker*)
      (error "GARRAY component requested does not exist."))
    (if (null (cdr indices))
        (garray-possible-indices (second axes) substructure)
      (garray-component-indices-aux substructure (cdr axes) (cdr indices))
      )))
      
(defmethod garray-possible-indices ((axis xa-axis) data)
  (declare (ignorable data))
  (loop for j from (axis-min axis) below (axis-limit axis) collect j))

(defmethod garray-possible-indices ((axis enum-axis) data)
  (declare (ignorable data))
  (copy-list (axis-possibles axis)))

(defmethod garray-possible-indices ((axis hash-axis) data)
  (lmaphash (lambda (key value) (declare (ignore value)) key) data))


(defun garray-component-garray (garray &rest indices)
  (let ((len (length indices))
        (rank (garray-rank garray))
        (axes (garray-axes garray))
        (data (garray-data garray))
        (*current-garray* garray)
        )
    (unless (< len rank)
      (error 
       (one-string
        "Invalid number of indices (~D) to GARRAY-COMPONENT-GARRAY. "
        "The number of indices must be less than the rank of the garray (~S).")
       len rank
       ))
    (garray-component-garray-aux data axes indices)
    ))

(defun garray-copy (garray) 
  #.(one-string-nl
     "Copies a garray. All the data structures that compose the GARRAY"
     "are copied, not shared. The elements of the garray are not, however,"
     "copied; they are simply transfered via SETQ.  E.g."
     "  (setq g1 (make-garray '(3)))"
     "  (setf (gref g1 0) #(a b c))"
     "  (setq g2 (copy-garray g1))"
     "  (eq (gref g1 0) (gref g2 0)) --> T")
  (garray-component-garray garray))
(defun copy-garray (garray) (garray-copy garray))

(defun garray-component-garray-aux (data axes indices)
  (if (null indices)
      (make-instance 
       'garray
       :rank (length axes)
       :axes (mapcar 'garray-copy-axis axes)
       :data (garray-copy-data-recursively (first axes) data (rest axes))
       :adjustable? (garray-adjustable? *current-garray*)
       :if-accessed-location-not-set 
       (garray-if-accessed-location-not-set *current-garray*)
       :create-arms? (garray-create-arms? *current-garray*)
       )
    (let ((substructure 
           (gref-aux-other-axis (first axes) (first indices) data)
           ))
      (garray-component-garray-aux substructure (rest axes) (rest indices))
      )))
      
(defmethod garray-copy-data-recursively ((axis num-axis) data remaining-axes)
  (let ((data-size (axis-extent axis))
        (last-axis? (null remaining-axes))
        (next-axis (first remaining-axes))
        (other-axes (rest remaining-axes))
        )
    (let ((copied-data 
           (make-array 
            (list data-size)
            :adjustable (garray-adjustable? *current-garray*)
            )))
      ;; Copy the substructure, if any
      (loop for j fixnum from 0 below data-size 
            as value = (aref data j) do
            (if (or last-axis? (eq value *garray-not-filled-in-yet-marker*))
                (setf (aref copied-data j) value)
              (setf (aref copied-data j)
                    (garray-copy-data-recursively next-axis value other-axes)
                    )))
      copied-data
      )))

(defmethod garray-copy-data-recursively ((axis hash-axis) data remaining-axes)
  (let* ((data-size (hash-table-count data))
         (last-axis? (null remaining-axes))
         (next-axis (first remaining-axes))
         (other-axes (rest remaining-axes))
         ;; Create the new hash, empty
         (copied-data (make-hash-table :test (axis-test axis) :size data-size))
         )
    ;; Copy the substructure, if any
    (maphash
     (lambda (key value)
       (setf (gethash key copied-data)
             (if last-axis?
                 value
               (garray-copy-data-recursively next-axis value other-axes)
               )))
     data)
    copied-data
    ))

(defmethod ref ((obj garray) index &rest indices)
  ;; (error "Doesn't work now because REF is 1-based and garrays are 0-based")
  (if (and (null indices) (consp index)) 
      (loop for i in index collect (gref obj i))
    (apply 'gref obj index indices)))

#||

(defmethod ref ((obj garray) index &rest indices)
  (if (null indices)
      ;; A single index, which might be a list of indices
      (if (consp index)
          (let ((results (mapcar (lambda (i) (gref obj i)) index)))
            (let ((g (make-garray (list (1+ (length results))))))
              (loop for r in results 
                    for j from 1 
                    do
                    (setf (gref obj j) r)
                    finally (return g)
                    )))
        (gref obj index))
    (let ((all-indices (cons index indices)))
      (if (range-components? all-indices) 
                    
          
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

||#

(defmethod (lisp:setf ref) ((new-value t) (obj garray) index &rest indices)
  (if (and (null indices) (consp index))
      (loop for i in index 
            for j from 0
            do (setf-gref obj (elt new-value j) i))
    (apply 'setf-gref obj new-value index indices)))

(defmethod cref ((obj garray) index &rest indices)
  (if (and (null indices) (consp index))
      (loop for i in index collect (gref obj i))
    (apply 'gref obj index indices)))

(defmethod (lisp:setf cref) ((new-value t) (obj garray) index &rest indices)
  (if (and (null indices) (consp index))
      (loop for i in index 
            for j from 0
            do (setf-gref obj (elt new-value j) i))
    (apply 'setf-gref obj new-value index indices)))

(defmethod iter-init ((obj garray)) 
  (let ((axes (garray-axes obj)))
    (cond
     ((and (= 1 (length axes)) (typep (first axes) 'xa-axis))
      (iter-init (garray-data obj)))
     (t 
      (let ((element-list nil))
        (gmap (lambda (x) (push x element-list)) obj)
        (iter-init (nreverse element-list))
        )))))



(defgeneric garray-convert-hash-index (hash-index hash-table-test)
  (:documentation 
   #.(one-string-nl
      "This generic function gets called any time any data item is"
      "to be inserted into or extracted from a garray with a hash axis."
      "This function is called on the HASH-INDEX provided by the user."
      "The result of this function is used as the actual hash index"
      "instead of HASH-INDEX."
      ""
      "(If a garray has more than one hash axis this function will be called"
      "once for each hash axis in the garray.)")))

(defmethod garray-convert-hash-index ((hash-index t) (hash-table-test t))
  hash-index)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *display-garray-conversion-mode* nil)
(defvar *display-garray-max-col-width* nil)

(defun new-display-garray 
       (g 
        &rest keys-and-values 
        &key 
        (output *standard-output*)
        (name "Unnamed")
        (type "GARRAY")
        (details? t)
        (starting-row 1)
        (starting-column 1)
        (max-rows nil)
        (max-columns nil)
        (specific-rows nil)
        (specific-columns nil)
        (maximum-column-width nil)
        (maximum-width nil)
        (invert? nil)
        (if-not-there? '-)
        (column-widths nil)
        (lines-between-rows 0)
        (spacing-between-columns 1)
        (alignment :left)
        (mode :normal)
        &allow-other-keys
        &aux 
        (rank (garray-rank g))
        )
  (declare
   (ignorable
    name type details? starting-row starting-column 
    max-rows max-columns
    specific-rows specific-columns maximum-column-width maximum-width
    invert? if-not-there? column-widths
    lines-between-rows spacing-between-columns alignment))
  (flet ((doit (stream) 
           (terpri stream)
           (describe-garray 
            g :stream stream :name name :type type :details? details?)
           (if (string-equal name "Unnamed")
               (format stream "~%Contents of ~A ~A: ~%~%" name type)
             (format stream "~%Contents of ~A ~A: ~%~%" type name))
           (cond
            ((= rank 1)
             (apply 'new-display-garray-1 g :stream stream keys-and-values))
            ((= rank 2) 
             (apply 'new-display-garray-2 g :stream stream keys-and-values))
            (t 
             (format 
              stream
              "~%Cannot display ~A contents of ~AS with > 2 dimensions yet~%"
              type type 
              )))))
    (let ((*display-garray-conversion-mode* mode)
          (*display-garray-max-col-width* maximum-column-width))
      (typecase output
        ((or pathname string) 
         (with-open-file (s output :direction :output :if-exists :supersede)
           (doit s)))
        (stream (doit output))
        (t (error "~S is not a filename, pathname, or a stream!"))
        ))))

(defun sorted-garray-indices (axis indices)
  (typecase axis 
    (xa-axis indices)
    ((or enum-axis hash-axis) 
     (sort (copy-list indices)
           (lambda (x y) 
             (let ((*display-garray-max-col-width* most-positive-fixnum))
               (string-lessp (keystring x) (keystring y))
               ))))))

(defun restrict-garray-indices (indices start max)
  (let* ((restricted-indices indices)
         (n (length restricted-indices)))
    (unless start (setq start 1))
    (if (> start n)
        (setq restricted-indices nil)
      (setq restricted-indices (subseq restricted-indices (1- start))))
    (when (> (length restricted-indices) (or max most-positive-fixnum))
      (setq restricted-indices (subseq restricted-indices 0 max)))
    restricted-indices
    ))

(defun invert-garray-data (data)
  (loop for j from 0 below (length (first data)) collect
        (mapcar (lambda (x) (elt x j)) data)
        ))

(defun new-display-garray-1 
       (g 
        &rest keys-and-values 
        &key 
        (invert? nil)
        (if-not-there? '-)
        (starting-row 1)
        (max-rows nil)
        (specific-rows nil)
        &allow-other-keys 
        &aux specific-keys 
        )
  (let* ((axis (first (garray-axes g)))
         (all-keys (sorted-garray-indices axis (garray-component-indices g))))
    (if specific-rows 
        (setq specific-keys specific-rows)
      (setq specific-keys 
            (restrict-garray-indices all-keys starting-row max-rows)
            ))
    (let* ((data
            (loop for key in specific-keys collect
                  (list 
                   (keystring key)
                   (value-string g if-not-there? key)
                   ))))
      (if (null invert?) 
          (apply 
           'write-table-rows data 
           :upper-left-alignment :right
           :first-column-alignment :right 
           ;; :first-row-alignment :left 
           keys-and-values
           )
        (apply 
         'write-table-rows
         (invert-garray-data data)
         :alignment :center
         keys-and-values
         )))))

(defun collect-2d-garray-data 
       (g first-axis-keys second-axis-keys if-not-there?)
  (loop for fakey in first-axis-keys 
        collect 
        (cons 
         (keystring fakey)
         (loop for sakey in second-axis-keys collect
               (value-string 
                g if-not-there? fakey sakey)
               ))))

;; Returns NIL if keys are disparate 
(defun find-minimal-set-of-keys (g first-axis-keys)
  (let* ((second-axis-keys-list 
          (loop for fakey in first-axis-keys collect
                (garray-component-indices g fakey)
                ))
      #| (max-length (reduce 'max (mapcar 'length second-axis-keys-list)))  |#
         (max-length 
             (IF second-axis-keys-list 
                 (reduce 'max (mapcar 'length second-axis-keys-list))))
         (most-keys-list 
          (find-if (lambda (x) (= (length x) max-length)) second-axis-keys-list)
          ))
    (when (every 
           (lambda (x) (null (set-difference x most-keys-list)))
           second-axis-keys-list
           )
      most-keys-list
      )))

(defun compute-printable-keys-row (keys)
  (cons 
   ""
   (mapcar 
    (lambda (x) (keystring x))
    keys
    )))

(defun new-display-garray-2
       (g 
        &rest keys-and-values 
        &key 
        (if-not-there? '-)
        (starting-row 1)
        (starting-column 1)
        (max-rows nil)
        (max-columns nil)
        (specific-rows nil)
        (specific-columns nil)
        (invert? nil)
        &allow-other-keys 
        )
  (unless starting-row (setq starting-row 1))
  (unless starting-column (setq starting-column 1))
  (let* ((axes (garray-axes g))
         (first-axis (first axes))
         (second-axis (second axes)))
    (cond
     ((typep second-axis 'utils::num-axis)
      (let* ((all-first-axis-keys 
              (sorted-garray-indices first-axis (garray-component-indices g)))
             (all-second-axis-keys 
              (sorted-garray-indices 
               second-axis 
               (garray-component-indices g (first all-first-axis-keys))
               ))
             (specific-first-axis-keys 
              (or specific-rows 
                  (restrict-garray-indices 
                   all-first-axis-keys starting-row max-rows)))
             (specific-second-axis-keys
              (or specific-columns
                  (restrict-garray-indices 
                   all-second-axis-keys starting-column max-columns)))
             (data 
              (cons
               (compute-printable-keys-row 
                specific-second-axis-keys)
               (collect-2d-garray-data 
                g specific-first-axis-keys specific-second-axis-keys
                if-not-there?
                ))))
        #+Debug
        (print (list 'af all-first-axis-keys 'as all-second-axis-keys 
                     'sfa specific-first-axis-keys 
                     'ssa specific-second-axis-keys
                     'data data))
        (apply 
         'write-table-rows 
         (if invert? (invert-garray-data data) data)
         :first-row-alignment :center 
         :first-column-alignment :right
         keys-and-values 
         )))
     (t 
      ;; figure out if the keys for each hash table along the second axis
      ;; are all subsets of the keys of the hash table with the most keys.  
      ;; If so, print the table in tabular form using the keys of the hash table
      ;; with the most keys, otherwise give up.  
      (let* ((all-first-axis-keys 
              (sorted-garray-indices first-axis (garray-component-indices g)))
             (specific-first-axis-keys 
              (or specific-rows 
                  (restrict-garray-indices 
                   all-first-axis-keys starting-row max-rows)))
             (second-axis-keys 
              (or specific-columns 
                  (find-minimal-set-of-keys g specific-first-axis-keys)
                  )))
        (if second-axis-keys 
            (let ((data 
                   (cons
                    (compute-printable-keys-row 
                     second-axis-keys)
                    (collect-2d-garray-data 
                     g specific-first-axis-keys second-axis-keys
                     if-not-there?
                     ))))
              (apply 
               'write-table-rows 
               (if invert? (invert-garray-data data) data)
               :first-row-alignment :center 
               :first-column-alignment :right
               keys-and-values 
               ))
          (let ((size (garray-current-total-size g)))
            (if (<= size 25)
                (utils::pprint-garray g *standard-output* nil)
              (format 
               t "<<Not a table! An array of disparate keys with ~D elements.>>"
               size
               )))
          ;; (error "Cannot display when second axis has disparate keys")
          ))))))


(defun keystring (key)
  (let ((s (handler-case 
               (garray-key-to-string key *display-garray-conversion-mode*)
             (error () "<KEY ERROR>")
             ))
        (mcw *display-garray-max-col-width*))
    (if mcw (maybe-clip-string s mcw) s)
    ))
  
(defun value-string (g if-not-there? &rest indices)
  (let ((s 
         (let ((value 
                (handler-case 
                    (apply 'gref g indices)
                  (error () if-not-there?)
                  )))
           (handler-case 
               (garray-value-to-string value *display-garray-conversion-mode*)
             (error () "<PRINT ERROR>")
             )))
        (mcw *display-garray-max-col-width*)
        )
    (if mcw (maybe-clip-string s mcw) s)
    ))

(defmethod garray-key-to-string ((key t) (mode t))
  (formatn "~S" key))

(defmethod garray-key-to-string ((key symbol) (mode (eql :vpl)))
  (formatn "~A" key))

(defmethod garray-value-to-string ((value t) (mode t))
  (formatn "~S" value))
  


        
