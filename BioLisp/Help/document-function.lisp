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

(defclass module-element ()
  ((module :initform nil :accessor module)))

(defclass parameter-documentation (documented)
  ((default-value    
    :initarg :default-value :initform nil :accessor default-value)
   (value-type
    :initarg :value-type :initform nil :accessor value-type)
   (parameter-type
    :initarg :parameter-type :initform :required :accessor parameter-type)
   (keyword-name
    :initarg :keyword-name :initform nil :accessor keyword-name)
   (synonyms
    :initarg :synonyms :initform nil :accessor synonyms)
   (mapping-style
    :initarg :mapping-style :initform nil :accessor mapping-style)
   (can-convert-from
    :initarg :can-convert-from :initform nil :accessor can-convert-from)))

(defmacro document-function (name &body (&rest docs))
  (labels ((meta (key) (find key docs :key 'first))
           (maybe-key (key &optional (f 'identity))
             (vwhen (data (meta key)) `(,key ',(funcall f (rest data))))))
    `(let ((thing (intern-documentation ',name 'function-documentation)))
       (document-function-internal
        ',name thing
        ,@(maybe-key :canonical 'first)
        ,@(maybe-key :flavor 'first)
        ,@(maybe-key :summary 'first)
        ,@(maybe-key :synonyms)
        ,@(maybe-key :returns)
        ,@(maybe-key :text)
        ,@(maybe-key :examples)
        ,@(maybe-key :see-also)
        ,@(maybe-key :syntax 'first)
        ,@(maybe-key :vpl-syntax)
        ,@(maybe-key :author)
        ,@(maybe-key :parameters)
        ,@(maybe-key :examples-package 'first)
        ,@(maybe-key :keywords)
        )
       ',name
       )))

;;; The verification and parsing function for DOCUMENT-FUNCTION
;;; rolled into one.  

(defun document-function-internal 
       (name thing &rest args &key &allow-other-keys)
  (flet ((meta (key) (vwhen (pos (position key args)) (nthcdr pos args))))
    (when (meta :canonical)
      (setf (canonical thing) 
            (make-instance 'function-documentation :name name))
      (setf thing (canonical thing)))
    (vwhen (data (meta :flavor)) (setf (flavor thing) (second data)))
    (vwhen (data (meta :summary)) (setf (docstring thing) (second data)))
    (vwhen (data (meta :synonyms)) (setf (synonyms thing) (second data)))
    (vwhen (data (meta :returns)) (setf (return-values thing) (second data)))
    (vwhen (data (meta :text)) (setf (text thing) (second data)))
    (vwhen (data (meta :examples))
      (setf 
       (examples thing) 
       (parse-document-function-examples (second data))))
    (vwhen (data (meta :see-also)) 
      (setf (see-also thing) 
            (progn 
              (multiple-value-bind (ok? verified-data)
                  (verify-see-also-entries (second data))
                (unless ok? 
                  (error "Problem parsing See-Also entries: ~A" data))
                (parse-see-also-entries verified-data)
                ))))
    (vwhen (data (meta :syntax)) (setf (syntax thing) (second data)))
    (vwhen (data (meta :vpl-syntax)) (setf (vpl-syntax thing) (second data)))
    (vwhen (data (meta :author)) (setf (author thing) (second data)))
    (vwhen (data (meta :parameters))
      (setf (parameters thing) 
            (parse-document-function-parameters (second data) thing)))
    (setf (examples-package thing)
          (intern 
           (if (meta :examples-package) 
               (symbol-name (second (meta :examples-package)))
             (package-name (symbol-package name)))
           :keyword))
    ;; make sure all the keywords are symbols or strings; ignore anything else. 
    (vwhen (data (meta :keywords)) 
      (setf (keywords thing) 
            (loop for keyword in (second data) 
                  when (or (symbolp keyword) (stringp keyword))
                  collect (keywordize keyword)
                  )))
    ;; Explicitly documented as opposed to created by interning as a
    ;; result of a DOCUMENT-MODULE form.
    (setf (explicitly-documented-p thing) t)
    (unless (function-in-any-module-except-uncategorized? name)
      (let ((uncategorized
             (intern-documentation *uncategorized-key* 'module)))
        ;; A function doesn't have a single module; the modules 
        ;; it belongs to are computed when its documentation is displayed.
        ;; (setf (module thing) uncategorized)
        (pushnew name (functions uncategorized))
        ))
    ))

(defmacro canonicalized-function-documentation-slot (name prefer)
  `(defmethod ,name ((doc function-documentation))
     ,(case prefer
        (:canonical
         `(or (and (canonical doc) (,name (canonical doc))) 
              (slot-value doc ',name)))
        (:supplemental
         `(or (slot-value doc ',name) 
              (and (canonical doc) (,name (canonical doc)))))
        (:merge
         `(append 
           (and (canonical doc) (,name (canonical doc))) 
           (slot-value doc ',name))))))

(canonicalized-function-documentation-slot docstring :supplemental)
(canonicalized-function-documentation-slot text :supplemental)
(canonicalized-function-documentation-slot keywords :canonical)
(canonicalized-function-documentation-slot see-also :merge)
(canonicalized-function-documentation-slot author :supplemental)
(canonicalized-function-documentation-slot explicitly-documented-p :canonical)

(canonicalized-function-documentation-slot module :canonical)

(defmethod return-values ((doc function-documentation))
  (let ((supplemental (slot-value doc 'return-values)))
    (cond
     ((canonical doc)
      (destructuring-bind 
          (&optional 
           canonical-return-desc 
           &key
           ((:type canonical-return-type) nil canonical-return-type-supplied-p)
           ((:display-type canonical-display-type) 
            nil canonical-display-type-supplied-p))
          (slot-value (canonical doc) 'return-values)
        (destructuring-bind
            (&optional 
             return-desc 
             &key
             ((:type return-type) nil)
             ((:display-type display-type) t))
            supplemental
          (list 
           (or canonical-return-desc return-desc)
           :type
           (if canonical-return-type-supplied-p
               canonical-return-type 
             return-type)
           :display-type 
           (if canonical-display-type-supplied-p 
               canonical-display-type
             display-type)))))
     (t supplemental))))

(canonicalized-function-documentation-slot syntax :canonical)
(canonicalized-function-documentation-slot vpl-syntax :canonical)
(canonicalized-function-documentation-slot examples :merge)
(canonicalized-function-documentation-slot examples-package :canonical)
(canonicalized-function-documentation-slot synonyms :merge)
(canonicalized-function-documentation-slot flavor :canonical)

(defmethod parameters ((doc function-documentation))
  (let ((supplemental (slot-value doc 'parameters)))
    (cond
      ((canonical doc)
       (loop for c in (parameters (canonical doc))
	  for s = (find (name c) supplemental :key #'name)
	  collect (merge-parameters c s)))
      (t supplemental))))

(defun merge-parameters (canonical supplemental)
  (if supplemental
      (let ((merged (make-instance 'parameter-documentation)))
	(loop for slot in 
	     '(name
	       docstring
	       text
	       keywords
	       see-also
	       author
	       explicitly-documented-p
	       default-value
	       value-type
	       parameter-type
	       keyword-name
	       synonyms
	       mapping-style
	       can-convert-from)
	   do (setf (slot-value merged slot)
		    (or (slot-value canonical slot)
			(slot-value supplemental slot))))
	(unless (value-type merged)
	  (setf (value-type merged) 'object))
	merged)
      canonical))
	 
(defun parse-document-function-parameters (parameters thing)
  (declare (ignore thing))
  (loop for p in parameters collect
       (apply #'make-instance 'parameter-documentation :name p)))

(defmacro undocument-function (name)
  `(let ((docs (find-documentation ',name 'function-documentation)))
     (when docs 
       (let ((module (module docs)))
         (when module 	
           (setf (functions module) (remove ',name (functions module))))
         (remove-documentation ',name 'function-documentation)
         ))))

(defmethod (setf module) :before (value (thing function-documentation))
  (let ((uncategorized (intern-documentation 'uncategorized 'module)))
    (when (and (eql (module thing) uncategorized)
               (not (eql value uncategorized)))
      (setf (functions uncategorized) 
            (remove (name thing) (functions uncategorized))))))


(defmethod shared-initialize 
           :after ((doc parameter-documentation) slot-names &key)
  (declare (ignore slot-names))
  (when (and (eql (parameter-type doc) '&key) (not (keyword-name doc)))
    (setf (keyword-name doc) (intern (symbol-name (name doc)) :keyword))))

(defmethod print-object ((object parameter-documentation) stream)
  (print-unreadable-object (object stream :type nil)
    (with-slots (name parameter-type default-value keyword-name) object
      (format 
       stream
       "~a :type ~a~@[ :default-value ~a~]~@[ :keyword-name: ~s~]"
       name parameter-type default-value keyword-name))))


(defun parse-ordinary-lambda-list (list)
  (loop with parameter-type = :required
       for item in list 
       until (eql item '&aux)
       when (lambda-list-keyword-p item) do (setf parameter-type item)
       else collect (normalize-param parameter-type item)))

(defgeneric normalize-param (type param))

(defmethod normalize-param ((type (eql :required)) param)
  (make-instance 'parameter-documentation :name param))

(defmethod normalize-param ((type (eql '&optional)) param)
  (let ((name (if (consp param) (first param) param))
	(default-value (if (consp param) (second param) nil)))
    (make-instance 
     'parameter-documentation 
     :name name :default-value default-value :parameter-type type)))

(defmethod normalize-param ((type (eql '&rest)) param) 
  (make-instance 'parameter-documentation :name param :parameter-type type))
  
(defmethod normalize-param ((type (eql '&key)) param)
  (let* ((name (if (consp param) 
                   (if (consp (car param)) (cadar param) (car param))
                 param))
         (default-value (if (consp param) (second param) nil))
         (keyword 
          (if (and (consp param) (consp (car param)))
              (caar param)
            (intern (symbol-name name) :keyword)))) 
    (make-instance 
     'parameter-documentation 
     :name name :default-value default-value 
     :parameter-type type :keyword-name keyword)))
    
(defun lambda-list-keyword-p (symbol)
  (member symbol lambda-list-keywords))

;; Access API

(defun dfdoc (symbol) 
  (find-documentation symbol 'function-documentation))

(defun function-documentation (symbol what)
  (slot-value (find-documentation symbol 'function-documentation) what))

(defun parameter-documentation (object what)
  (slot-value object what))
