;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

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

(defvar *subtemplate* nil)
(defvar *parent* nil)
(defvar *property-list* nil)
(defvar *subtemplate-args* nil)
(defvar *new-snippet-id?* t)
(defvar *copy-in-progress* nil)

(defvar *disable-snippet-initialization* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun inherit (property parent-classes) 
    (let ((values nil)) 
      (if (null parent-classes) 
          values 
        (loop for pc in parent-classes do 
              (setq values (union values (get pc property)))
              (setq 
               values 
               (union values (inherit property (get pc :parent-classes))))
              finally (return values)
              )))))

(defclass snippet () 
  ((id :accessor snippet-id :initarg :id :initform nil)
   (parent :accessor snippet-parent :initarg :parent :initform nil)
   (children :accessor snippet-children :initarg :children :initform nil)
   (value :accessor snippet-value :initarg :value :initform nil)
   (properties :accessor snippet-properties :initarg :properties :initform nil)
   ))

(defmethod initialize-instance :after ((obj snippet) &rest initargs)
  (declare (ignore initargs))
  (unless *disable-snippet-initialization* 
    ;; (vdbg "In initialize-instance of snippet type ~A~%" (snippet-type obj))
    (when *new-snippet-id?*
      (setf (snippet-id obj) (new-unique-id :snippet-id))
      (register-snippet obj))
    ;; (vdbg "Registered snippet...~%")
    (setf (snippet-parent obj) *parent*)
    (setf (snippet-properties obj) *property-list*)
    (set-snippet-property obj :template *subtemplate*)
    ;; (vdbg "Done initialize-instance~%")
    ))

(defun snippet-type (snippet) (type-of snippet))
(defun snippet-plist (snippet) (snippet-properties snippet))

(defmethod print-object ((obj snippet) stream)
  (format 
   stream "<SNP TYPE: ~A ID: ~A PARENT: ~A VALUE: ~S # OF CHILDREN: ~S>"
   (snippet-type obj) (snippet-id obj) 
   (vwhen (p (snippet-parent obj)) (snippet-id p))
   (snippet-value obj)
   (length (snippet-children obj))
   ))

(defun treeprint-snippet (s &optional (stream t) (indent 0))
  (flet ((indent (n) (dotimes (j n) (format stream " "))))
    (indent indent)
    (format 
     stream 
     (typecase s
       (uniform-choice-snippet "<SNP TYPE: ~A ID: ~A PARENT: ~A~%VALUE: ~S>~%")
       (otherwise "<SNP TYPE: ~A ID: ~A PARENT: ~A VALUE: ~S>~%"))
     (snippet-type s) (snippet-id s) 
     (vwhen (p (snippet-parent s)) (snippet-id p))
     (snippet-value s)
     )
    (let ((indent (+ indent 2)))
      (indent indent)
      (when (typep s 'return-value-snippet)
        (format stream "RVAL: ~S " (snippet-return-type s)))
      (when (typep s 'labeled-snippet)
        (format stream "LABEL: ~S " (snippet-label s)))
      (format stream "PROPERTIES: ~S~%" (snippet-properties s))
      (loop for c in (snippet-children s) do 
            (treeprint-snippet c stream indent)
            ))))

(defun ts (s &optional (stream t)) (treeprint-snippet s stream 0))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-snippet-type
          (name parent-classes &key (slots nil) (keywords nil) (flags nil))
  `(progn
     (defclass ,name (,@parent-classes) ,slots)
     (eval-when (:Compile-toplevel :load-toplevel :execute)
       (setf (get ',name :parent-classes) ',parent-classes)
       (setf (get ',name :template-keys)
             ',(union keywords (inherit :template-keys parent-classes)))
       (setf (get ',name :template-flags)
             ',(union flags (inherit :template-flags parent-classes)))
       )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *allowed-template-keys* 
    '(:reveal-label :htype :description))
  (defparameter *allowed-template-flags* 
    '(:newline :splice :hidden-node))
  )

(defun legal-keys-and-flags (snippet-type)
  (values 
   (union *allowed-template-keys* (get snippet-type :template-keys))
   (union *allowed-template-flags* (get snippet-type :template-flags))
   ))

(defmacro def-subtemplate-alias 
          (alias-keyword subtemplate-keyword &rest keys-and-flags)
  `(progn 
     (setf (get ',alias-keyword :subtemplate-alias-expander) 
           (lambda (subtemplate) 
             (append 
              (cons ,subtemplate-keyword (rest subtemplate))
              ',keys-and-flags
              )))))

(defparameter *snippet-keywords->snippet-types-map*
  '((:constant constant-snippet)
    (:literal literal-snippet)
    (:symbol symbol-snippet)
    (:form value-form-snippet)
    (:arg argument-snippet)
    (:macro  call-snippet)
    (:function call-snippet)
    (:define-function call-snippet)
    (:progn progn-snippet)
    (:agg aggregate-snippet)
    (:choice uniform-choice-snippet)
    (:keys-and-flags keys-and-flags-snippet)
    (:toplevel-output toplevel-output-snippet)
    (:output-value output-value-snippet)
    (:keyword keyword-snippet)
    (:flag flag-snippet)
    ))

(defun snippet-keyword->snippet-type (keyword)
  (cadr (assoc keyword *snippet-keywords->snippet-types-map*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
(def-snippet-type
 return-value-snippet (snippet)
 :slots                   
 ((return-type
   :accessor snippet-return-type :initarg :return-type :initform nil)))

(def-snippet-type
 labeled-snippet (snippet) 
 :slots 
 ((label :accessor snippet-label :initarg :label :initform "")))

(def-snippet-type structural-snippet (snippet))

;;; Leaf snippets 

(def-snippet-type leaf-snippet (snippet))
(def-snippet-type
 literal-snippet (leaf-snippet) 
 :keywords (:class) :flags nil)
(def-snippet-type cs-snippet (leaf-snippet return-value-snippet))
(def-snippet-type constant-snippet (cs-snippet))
(def-snippet-type symbol-snippet (cs-snippet))
(def-snippet-type symbol-place-snippet (symbol-snippet))
(def-snippet-type symbol-rvalue-snippet (symbol-snippet))
(def-snippet-type symbol-arg-snippet (symbol-snippet))

(defmethod snippet-label ((obj leaf-snippet)) (string (snippet-value obj)))
(defmethod snippet-label ((obj symbol-snippet))
  (string-downcase (string (snippet-value obj))))

;;; Hole (form) snippets

(def-snippet-type
 form-snippet (labeled-snippet)
 :keywords (:insert-type))
(def-snippet-type value-form-snippet (form-snippet return-value-snippet))

(def-snippet-type
 argument-snippet (form-snippet structural-snippet labeled-snippet)
 :flags (:place))

(def-subtemplate-alias :place :arg :place)


;;; Call snippets

(def-snippet-type 
 call-snippet (labeled-snippet return-value-snippet)
 :flags (:call-type))

;;; Miscellaneous snippets found in templates

(def-snippet-type 
 progn-snippet (structural-snippet)
 :flags (:delete-box :main-menu))
                  

(def-subtemplate-alias :sprogn :progn :splice)

;;; Aggregate snippets 

(def-snippet-type
 aggregate-snippet (labeled-snippet)
 :keywords (:display-one-hole :display-two-holes :options-label)
 :flags (:splice :one-form-required))

(def-subtemplate-alias :zero :agg :display-one-hole t)
(def-subtemplate-alias :zero-espliced :agg :display-one-hole t :splice)
(def-subtemplate-alias :&rest :zero-espliced)
(def-subtemplate-alias :&rest-noshow :agg :splice)
(def-subtemplate-alias
 :zero-ispliced :agg 
 :display-one-hole t)
(def-subtemplate-alias
 :zero-dspliced :agg
 :display-one-hole t :splice)

;; :body cannot take any keys or flags!
(def-subtemplate-alias :body :zero-espliced "forms" (:form "form" t))

(def-subtemplate-alias
 :one :agg
 :one-form-required :display-one-hole t)
(def-subtemplate-alias
 :one-espliced :agg 
 :one-form-required :display-one-hole t :splice)

(def-subtemplate-alias 
 :two :agg
 :one-form-required :display-two-holes t)

(def-subtemplate-alias 
 :two-espliced :agg
 :one-form-required :display-two-holes t :splice)

;;; Choice snippets 

#||

Choices have the following properties: 

  -- Literal vs. instantiated.  Whether the choice is simply some text to
be inserted into the display or a subtemplate to be instantiated and
then possibly filled in by the user.

  -- Optional vs. required.  Whether a choice must be selected by the
user before the code can execute.

  -- Multiple vs. single.  Whether the user can select more than one
choice or is limited to a single selection.

  -- Repeatable vs. non-repeatable.  Whether the user can select an
already- selected choice, creating more than one of the same choice.

Tokens are examples of a choice with the :literal, :optional, :single
and :non-repeatable properties.

An optional argument in the lisp sense is an example of a choice with
the :instantiated, :optional, :single, and :non-repeatable properties.

An optional loop clause such as 'by <value>' is another example with
the same properties, as is an aggregate clause for LOOP and FOR-EACH.

The loop control for a LOOP is an example of a choice with
the :instantiated, :required, :multiple, and :repeatable properties.

The primary iteration clause for a FOR-EACH is an example of a choice
with the :instantiated :required :single and :non-repeatable properties.

The loop control for a FOR-EACH is an example of a choice with the
:instantiated, :optional, :multiple, and :repeatable properties.

The keys-and-flags is unfortunately none of the above.  It has to be
handled specially because the keys are templates while the flags are
literals.

||#

(def-snippet-type choice-snippet (snippet))

(defmethod print-object ((obj choice-snippet) stream) 
  (format 
   stream "<SNP TYPE: ~A ID: ~A PARENT: ~S # OF CHILDREN: ~S ~%"
   (snippet-type obj) (snippet-id obj) 
   (vwhen (p (snippet-parent obj)) (snippet-id p))
   (length (snippet-children obj)))
  (format stream "  VALUE: ~S>~%" (snippet-value obj)))

(def-snippet-type keys-and-flags-snippet (choice-snippet))

(defmethod snippet-label ((obj keys-and-flags-snippet)) "Options")

(def-snippet-type uniform-choice-snippet (choice-snippet labeled-snippet)
   :keywords (:choice-type :class :default :display-one-hole :show-first-choice) ;********
   :flags (:literal :instantiated :optional :required :multiple :single 
			 :repeatable :non-repeatable)) ; show-first-choice ; *******

(def-subtemplate-alias
 :simple-required-choice :choice
 :choice-type :simple-choice :class :define-function-token
 :literal :required :single :non-repeatable)

#+maybe-later
(def-subtemplate-alias
 :simple-optional-choice :choice
 :choice-type :simple-choice :class :define-function-token
 :literal :optional :single :non-repeatable)

(def-subtemplate-alias
 :token :choice
 :choice-type :token :class :define-function-token 
 :literal :optional :single :non-repeatable)

(def-subtemplate-alias
 :optional-arg :choice
 :choice-type :optional-arg 
 :instantiated :optional :single :non-repeatable)

(def-subtemplate-alias
 :optional-labeled-arg :choice
 :choice-type :optional-labeled-arg
 :instantiated :optional :single :non-repeatable 
 :splice)

(def-subtemplate-alias
 :optional-labeled-clause :choice
 :choice-type :optional-labeled-clause
 :instantiated :optional :single :non-repeatable
 :splice)

(def-subtemplate-alias
 :loop-iterator :choice
 :choice-type :loop-iterator
 :instantiated :required :multiple :repeatable
 :splice)

(def-subtemplate-alias
 :for-each-primary-iterator :choice
 :choice-type :for-each-primary-iterator
 :instantiated :required :single :non-repeatable
 :splice)

(def-subtemplate-alias
 :define-function-options :choice 
 :choice-type :define-function-options 
 :instantiated :optional :multiple :non-repeatable)

(def-subtemplate-alias :loop-agg :optional-labeled-clause)

(def-subtemplate-alias
 :var-or-list-of-vars :choice 
 :choice-type :var-or-list-of-vars  
 :instantiated :required :single :non-repeatable :show-first-choice t ; ********
 :display-one-hole t )   ;**********

;;; Miscellaneous snippets not found in templates

(def-snippet-type output-snippet (snippet))
(def-snippet-type toplevel-output-snippet (output-snippet labeled-snippet))
(def-snippet-type output-value-snippet (output-snippet return-value-snippet))

(def-snippet-type
 keyword-snippet (structural-snippet) 
 :keywords (:default :possible-values) :flags nil)
(def-snippet-type flag-snippet (leaf-snippet))

(def-snippet-type toplevel-ws-snippet (snippet))
(def-snippet-type toplevel-rs-snippet (snippet))

