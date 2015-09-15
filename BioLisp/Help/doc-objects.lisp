;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.


;;; All the various types of documentation (theoretically) available
;;; in the system.

;;; Arguably, FUNCTION-DOCUMENTATION and SYMBOL-DOC should be merged

(defparameter *documentation-types*
  '(
    documentation-file
    function-documentation
    glossary-entry
    ;; macro-documentation 
    module
    symbol-doc
    topic
    tutorial
    ;; variable-documentation
    ))

(defparameter *doc-types-hash-types*
  '(
    (documentation-file equal)
    (function-documentation eq)
    (glossary-entry equalp)
    ;; macro-documentation 
    (module equalp)
    (symbol-doc eq)
    (topic equalp)
    (tutorial equalp)
    ;; variable-documentation
    ))

;; Where all documentation objects are stored.
;; Use FIND-DOCUMENTATION to pull something out

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-doc-hash-tables ()
    (let ((ht (make-hash-table)))
      (loop for (doc-type hash-test) in *doc-types-hash-types* do
            (setf (gethash doc-type ht) (make-hash-table :test hash-test)))
      ht
      )))

(defvar *documentation* (create-doc-hash-tables))

(defun intern-documentation (name type)
  (or (find-documentation name type)
      (setf (gethash name (gethash type *documentation*)) 
            (make-instance type :name name))))

(defun remove-documentation (name type)
  (remhash name (gethash type *documentation*)) (make-instance type :name name))

(defun find-documentation (name type)
  (gethash name (gethash type *documentation*)))

(defun clear-documentation ()
  (setf *documentation* (create-doc-hash-tables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The hierarchy of documentation classes

(defclass basicdoc ()
  ((name :initarg :name :accessor name)
   ;; AKA 'summary'
   (docstring :initform nil :initarg :docstring :accessor docstring)
   (referred-to-by :initform nil :accessor referred-to-by)
   ))

(defmethod summary ((obj basicdoc)) (docstring obj))
(defmethod text ((obj basicdoc)) nil)
(defmethod keywords ((obj basicdoc)) nil)
(defmethod see-also ((obj basicdoc)) nil)
(defmethod explicitly-documented-p ((obj basicdoc)) nil)
(defmethod author ((obj basicdoc)) nil)

(defmethod print-object ((obj basicdoc) stream)
  (format stream "<Docobj ~A (~A)>" (help:name obj) (type-of obj)))

(defclass documented (basicdoc)
  ((text      :initform nil :accessor text)
   (keywords  :initform nil :accessor keywords)
   (see-also  :initform nil :accessor see-also)
   (author    :initform nil :accessor author)
   (explicitly-documented-p :initform nil :accessor explicitly-documented-p)))

(defclass mode-documented (documented) 
  ((display-modes 
    :initform (list :all) 
    :accessor display-modes
    )))

(defclass documentation-file (mode-documented) 
  ((label :initform nil :accessor label)
   (source-file :initform nil :accessor source-file)
   (associated-text-file 
    :initform nil
    :accessor associated-text-file
    )
   (matches :initform nil :accessor matches)
   (descriptor :initform nil :accessor descriptor)
   ))
                         
;; the reader methods are defined in document-function.lisp
(defclass function-documentation (documented module-element)
  ((parameters    :initform nil :writer (setf parameters))
   (return-values :initform nil :writer (setf return-values))
   (syntax        :initform nil :writer (setf syntax))
   (vpl-syntax    :initform nil :writer (setf vpl-syntax))
   (examples      :initform nil :writer (setf examples))
   (examples-package :initform nil :writer (setf examples-package))
   (synonyms      :initform nil :writer (setf synonyms))
   (flavor        :initform :defun :writer (setf flavor))
   (canonical     :initform nil :accessor canonical)
   (aliased       :initform nil :accessor aliased)
   ))


(defmethod print-object ((obj function-documentation) stream)
  (print-symbol-docobj obj stream "DocFunc"))

(defclass glossary-entry (documented) ())

;; If/when actually implemented, should become a subtype of DOCUMENTED
(defclass macro-documentation (basicdoc) ())

(defclass module (mode-documented)
  ((functions :initform nil :accessor functions)
   (variables :initform nil :accessor variables)
   (macros    :initform nil :accessor macros)
   (submodules :initform nil :accessor submodules)
   (toplevel? :initform t :accessor toplevel?)
   (alpha-listing? :initform t :accessor alpha-listing?)
   ))

(defclass symbol-doc (basicdoc) 
  (
   ;; one of :special-operator, :define-function, :macro, :function,
   ;; :constant, :variable, or :type
   (stype :initform nil :initarg :stype :accessor stype)
   ;; one of :function, :variable, or :type
   (dtype :initform nil :initarg :dtype :accessor dtype)))

(defmethod print-object ((obj symbol-doc) stream)
  (print-symbol-docobj obj stream "Symbol"))

(defclass topic (mode-documented) ())

(defclass tutorial (mode-documented) 
  ;; :filename -- a string, must be full pathname 
  ;; :file-type -- either :html or :lhtml 
  ;; :user-mode -- a keyword or a list of keywords
  ;; :sort-order -- an integer 
  ;; :description -- a string, this is really the summary 
  ;; :section-header -- two strings, a title, and a color
  ;; :lhtml-function -- used only wth file type lhtml, must be a symbol
  ;; :start-function -- used only with file type :lhtml, must be a symbol
  ((filename :initform nil :accessor filename)
   (file-type :initform nil :accessor file-type)
   (user-mode :initform nil :accessor user-mode)
   (sort-order :initform nil :accessor sort-order)
   (description :initform nil :accessor description)
   (lhtml-function :initform nil :accessor lhtml-function)
   (start-function :initform nil :accessor start-function)
   (section-header :initform nil :accessor section-header)
   ))

;; If/when actually implemented, should become a subtype of DOCUMENTED
(defclass variable-documentation (basicdoc) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The definitions that create the verifiers and parsers for
;;; the definition forms for each documentation object.


(define-doc-definer 
 documentation-file 
 def-documentation-file
 create-documentation-file
 ((:summary :one-or-none ddd-string-or-nil identity help:docstring)
  (:keywords :list ddd-all-symbols-or-strings identity help:keywords)
  (:see-also :list verify-see-also-entries parse-see-also-entries help:see-also)
  (:author :list ddd-all-strings identity help:author)
  (:descriptor :one-or-none ddd-string-or-nil identity help:descriptor)
  ))

;; function-documentation has no define-doc-definer, its verifer and parser
;; are implemented by hand in document-function.lisp

(define-doc-definer 
 glossary-entry 
 def-glossary-entry 
 create-glossary-entry
 ((:summary :one-or-none ddd-string-or-nil identity help:docstring)
  (:text :non-nil-list ddd-identity identity help:text)
  (:keywords :list ddd-all-symbols-or-strings identity help:keywords)
  (:see-also :list verify-see-also-entries parse-see-also-entries help:see-also)
  (:author :list ddd-all-strings identity help:author)
  ))

;; not doing macro-documentation for now since it is not used 

(define-doc-definer 
 module  
 def-module
 create-module
 ((:summary :one-or-none ddd-string-or-nil identity help:docstring)
  (:text :non-nil-list ddd-identity identity help:text)
  (:keywords :list ddd-all-symbols-or-strings identity help:keywords)
  (:see-also :list verify-see-also-entries parse-see-also-entries help:see-also)
  (:author :list ddd-all-strings identity help:author)
  (:functions :list ddd-all-symbols identity help:functions)
  (:variables :list ddd-all-symbols identity help:variables)
  (:macros :list ddd-all-symbols identity help:macros)
  (:submodules :list ddd-all-symbols identity help:submodules)
  (:toplevel? :exactly-one ddd-boolean identity help:toplevel?)
  (:alpha-listing? :exactly-one ddd-boolean identity help:alpha-listing?)
  (:display-modes :list ddd-all-symbols identity help:display-modes)
  )
 :after-code (setf (explicitly-documented-p obj) t))

(defmacro document-module (name &body (docstring &rest meta))
  `(def-module ,(string name) (:summary ,docstring) ,@meta))

(defmacro undocument-module (name &key remove-functions)
  `(progn
     (let ((module (find-documentation ',name 'module)))
       (if ,remove-functions
	   (loop for fn in (functions module) do 
                 (remove-documentation fn 'function-documentation))
	   (let ((uncategorized (intern-documentation 'uncategorized 'module)))
	     (loop for fn in (functions module)
		for fn-doc = (find-documentation fn 'function-documentation)
		when fn-doc do
		  (setf (module fn-doc) uncategorized)
		  (push fn (functions uncategorized))))))
     (remove-documentation ',name 'module)))

(defun modules () (hash-table-values (gethash 'module *documentation*)))

(defparameter *uncategorized-key* "UNCATEGORIZED")

;;; Setup the Uncategorized module.
(let ((uncategorized (intern-documentation *uncategorized-key* 'module)))
  (setf (docstring uncategorized) 
        "Documented elements not part of any other module."))

;; No symbol-doc creator macro because symbol-doc entries are reserved for
;; those exported symbols which do not have define-function entries.  These
;; symbols are to be searched out at the end of the system load and
;; at that point symbol-doc objects are created for each such symbol
;; (using the below function)

(defun create-symbol-doc (symbol &key docstring dtype stype)
  (make-instance
   'help:symbol-doc 
   :name symbol :docstring docstring :dtype dtype :stype stype))

(defun create-symbol-doc-entries (&key (mode :external))
  (declare (ignore mode))
  (loop 
   with hash = (gethash 'help:symbol-doc *documentation*)
   with packages-not-to-search =
   (remove (find-package :cl-user) cl-user::*startup-packages*)
   with cl-package = (find-package :common-lisp)
   for package in (list-all-packages) 
   do
   ;; The startup packages are those that exist at the start
   ;; of our system load.  Hence we only look for symbols in
   ;; our own packages, CL, and third party stuff we load, like PPCRE
   (unless (and (member package packages-not-to-search) 
                (not (eq package cl-package)))
     (do-external-symbols (symbol package) 
       (when (or (eq package cl-package) 
                 (not (eq (symbol-package symbol) cl-package)))
         (cond
          ((get symbol :alias-of) (create-alias-for symbol))
          (t 
           (vwhen (docs (maybe-create-symbol-docs symbol))
             (setf (gethash symbol hash) docs)
             ))))))))


;; create a dummy function-documentation object whose only meaningful slots
;; are explicitly-documented-p, which is given the value :alias-of to denote
;; that this is a dummy, and see-also, which contains the real function
;; that the symbol is an alias for.  
(defun create-alias-for (symbol)
  (let ((real-function (get symbol :alias-of))
        (docobj (intern-documentation symbol 'help:function-documentation)))
    (setf (explicitly-documented-p docobj) :alias-of)
    (setf (docstring docobj) (formatn "Alias for ~A" real-function))
    (setf (see-also docobj) nil)
    ;; (list (find-documentation real-function 'help:function-documentation))
    ))
    

;;; Create a set of HELP:SYMBOL-DOC data structures, for a symbol

(defun maybe-create-symbol-docs (symbol) 
  (remove-if 
   'null 
   (list 
    (when (fboundp symbol) 
      ;; Don't create an entry if the symbol is already
      ;; documented by DOCUMENT-FUNCTION
      (unless (find-documentation symbol 'help:function-documentation)
        (create-symbol-doc
         symbol
         :docstring (documentation symbol 'function)
         :stype 
         (cond 
          ((special-operator-p symbol) :special-operator)
          ((define-function-p symbol) :define-function)
          ((macro-function symbol) :macro)
          (t :function))
         :dtype :function
         )))
    (when (boundp symbol)
      (create-symbol-doc
       symbol 
       :docstring (documentation symbol 'variable) 
       :stype
       (cond
        ((constantp symbol) :constant)
        (t :variable))
       :dtype :variable
       ))
    ;; Figure out if a symbol denotes a type.  Not portable.
    ;; This type checking causes the autoloading of the stream goo in ACL.
    (ignore-errors 
      (typep nil symbol)
      (create-symbol-doc
       symbol 
       :docstring (documentation symbol 'type)
       :stype :type
       :dtype :type
       )))))
        
(define-doc-definer 
 topic 
 def-topic 
 create-topic
 ((:summary :one-or-none ddd-string-or-nil identity help:docstring)
  (:text :non-nil-list ddd-identity identity help:text)
  (:keywords :list ddd-all-symbols-or-strings identity help:keywords)
  (:see-also :list verify-see-also-entries parse-see-also-entries help:see-also)
  (:author :list ddd-all-strings identity help:author)
  ))

;; The define-doc-definer for tutorials is in live-tutorial.lisp

;; not doing variable-documentation for now since it is not used 

#+not-used
(defmacro document-variable (name docstring)
  `(let ((thing (intern-documentation ',name 'variable-documentation)))
     (setf (explicitly-documented-p thing) t)
     (setf (docstring thing) ,docstring)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each documentation type has an associated URL which displays
;; the documentation object identified by the parameters of the URL
;; (which are taken from the SEE-ALSO data structure).  See-also 
;; lists can also contain objects which are not documentation-objects
;; per se (e.g., Common Lisp hyperspec references, URLs and frames) 

(wb::define-url&pkg&args 
 help-documentation-file-url
 "/new-help/help-documentation-file-url" :name)

;; File documentation doesn't have its own URL because the documentation
;;   directory already has its own AllegroServe PUBLISH-DIRECTORY url

(wb::define-url&pkg&args
 help-function-documentation-url 
 "/new-help/help-function-documentation-url" :name :package)

(wb::define-url&pkg&args
 help-glossary-entry-url "/new-help/help-glossary-entry-url" :name)

;; not doing macro-documentation because it's not used.  

(wb::define-url&pkg&args
 help-module-url "/new-help/help-module-url" :name)

(wb::define-url&pkg&args
 help-symbol-doc-url 
 "/new-help/help-symbol-doc-url" :name :package :type)

(wb::define-url&pkg&args
 help-topic-url "/new-help/help-topic-url" :name)

(wb::define-url&pkg&args
 help-tutorial-url "/new-help/help-tutorial-url" :name)

;; not doing variable-documentation because it's not used.  

;; URLs don't have their own URL because they are already a URL!
;; Frames don't have their own URL here because one exists already.

;;; A page which lists all the glossary entries

(wb::define-url&pkg&args
 help-glossary-url "/new-help/help-glossary-url")

;;; A page which lists all the pertinent modules

(wb::define-url&pkg&args help-modules-url "/help/modules")



